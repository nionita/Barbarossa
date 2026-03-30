from __future__ import annotations

import importlib.util
import shutil
import sys
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest import mock


MODULE_PATH = Path(__file__).resolve().parents[1] / "scripts" / "run_long_sprt.py"
SPEC = importlib.util.spec_from_file_location("run_long_sprt", MODULE_PATH)
assert SPEC is not None
assert SPEC.loader is not None
sprt = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = sprt
SPEC.loader.exec_module(sprt)


class RunLongSprtTests(unittest.TestCase):
    def setUp(self) -> None:
        scratch_root = Path(__file__).resolve().parent / "_codex_scratch"
        scratch_root.mkdir(exist_ok=True)
        self.root = scratch_root / self.id().rsplit(".", 1)[-1]
        if self.root.exists():
            shutil.rmtree(self.root)
        self.root.mkdir()
        self.base_dir = self.root / "base"
        self.base_dir.mkdir()
        self.selfplay_path = self.root / "SelfPlay"
        self.selfplay_path.write_text("#!/bin/sh\n", encoding="utf-8", newline="\n")

    def tearDown(self) -> None:
        shutil.rmtree(self.root)

    def make_layout(self) -> None:
        for name in sprt.RUN_BUCKETS:
            (self.base_dir / name).mkdir(exist_ok=True)

    def write_config(self, text: str) -> None:
        (self.base_dir / "config.ini").write_text(text, encoding="utf-8", newline="\n")

    def write_candidate(self, name: str = "candidate.txt") -> Path:
        path = self.base_dir / "candidates" / name
        path.write_text("weights\n", encoding="utf-8", newline="\n")
        return path

    def archived_run(self, bucket: str, run_name: str) -> Path:
        dirs = sorted((self.base_dir / bucket).glob(f"{run_name}*"))
        self.assertEqual(len(dirs), 1)
        return dirs[0]

    def fake_completed(self, returncode: int, stdout_text: str = "", stderr_text: str = "", save_text: str | None = None):
        def runner(command, cwd, check, stdout, stderr, text):
            del command, check, text
            stdout.write(stdout_text)
            stderr.write(stderr_text)
            if save_text is not None:
                (Path(cwd) / sprt.SAVE_FILE_NAME).write_text(save_text, encoding="utf-8", newline="\n")
            return SimpleNamespace(returncode=returncode)

        return runner

    def test_load_selfplay_args_accepts_sectionless_ini(self) -> None:
        config_path = self.base_dir / "config.ini"
        config_path.write_text(
            "input = /tmp/fens.txt\nnodes = 10000\nsprt_alpha = 0.05\nparam = foo=1\n  bar=2\n",
            encoding="utf-8",
            newline="\n",
        )
        args = sprt.load_selfplay_args(config_path)
        self.assertEqual(
            args,
            [
                "-i",
                "/tmp/fens.txt",
                "-n",
                "10000",
                "--sprt-alpha",
                "0.05",
                "-p",
                "foo=1",
                "-p",
                "bar=2",
            ],
        )

    def test_h0_run_moves_to_rejected(self) -> None:
        self.make_layout()
        self.write_config("input = /tmp/fens.txt\nnodes = 10000\n")
        self.write_candidate()

        with mock.patch.object(sprt.subprocess, "run", side_effect=self.fake_completed(0, "Termination: SPRT accepted H0\n")):
            code = sprt.main([str(self.selfplay_path), str(self.base_dir)])

        self.assertEqual(code, 0)
        archived = self.archived_run("rejected", "candidate")
        self.assertTrue((archived / "candidate.txt").exists())
        self.assertFalse(any((self.base_dir / "running").iterdir()))

    def test_h1_run_moves_to_accepted(self) -> None:
        self.make_layout()
        self.write_config("input = /tmp/fens.txt\nnodes = 10000\n")
        self.write_candidate()

        with mock.patch.object(sprt.subprocess, "run", side_effect=self.fake_completed(0, "SPRT: accepted H1\n")):
            code = sprt.main([str(self.selfplay_path), str(self.base_dir)])

        self.assertEqual(code, 0)
        archived = self.archived_run("accepted", "candidate")
        self.assertTrue((archived / "candidate.txt").exists())

    def test_nonzero_run_moves_to_fatal(self) -> None:
        self.make_layout()
        self.write_config("input = /tmp/fens.txt\nnodes = 10000\n")
        self.write_candidate()

        with mock.patch.object(sprt.subprocess, "run", side_effect=self.fake_completed(7, "oops\n", "boom\n")):
            code = sprt.main([str(self.selfplay_path), str(self.base_dir)])

        self.assertEqual(code, 1)
        archived = self.archived_run("fatal", "candidate")
        self.assertTrue((archived / "candidate.txt").exists())

    def test_undecided_run_moves_to_undecided(self) -> None:
        self.make_layout()
        self.write_config("input = /tmp/fens.txt\nnodes = 10000\n")
        self.write_candidate()
        save_text = "-- save\nverdict = SprtContinue\n"

        with mock.patch.object(sprt.subprocess, "run", side_effect=self.fake_completed(0, "no final verdict\n", save_text=save_text)):
            code = sprt.main([str(self.selfplay_path), str(self.base_dir)])

        self.assertEqual(code, 0)
        archived = self.archived_run("undecided", "candidate")
        self.assertTrue((archived / sprt.SAVE_FILE_NAME).exists())

    def test_resume_prefers_existing_running_candidate(self) -> None:
        self.make_layout()
        self.write_config("input = /tmp/fens.txt\nnodes = 10000\n")
        queued = self.write_candidate("queued.txt")
        run_dir = self.base_dir / "running" / "resumable"
        run_dir.mkdir()
        running_candidate = run_dir / "resumable.txt"
        running_candidate.write_text("resume\n", encoding="utf-8", newline="\n")
        observed: list[list[str]] = []

        def runner(command, cwd, check, stdout, stderr, text):
            del cwd, check, text
            observed.append(command)
            stdout.write("Termination: SPRT accepted H1\n")
            stderr.write("")
            return SimpleNamespace(returncode=0)

        with mock.patch.object(sprt.subprocess, "run", side_effect=runner):
            code = sprt.main([str(self.selfplay_path), str(self.base_dir)])

        self.assertEqual(code, 0)
        self.assertEqual(len(observed), 1)
        command = observed[0]
        self.assertIn(str(running_candidate), command)
        self.assertTrue(queued.exists())
        archived = self.archived_run("accepted", "resumable")
        self.assertTrue((archived / "resumable.txt").exists())


if __name__ == "__main__":
    unittest.main()
