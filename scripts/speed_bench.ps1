# Speed comparison of two SelfPlay perf builds (deterministic flavour, scripts/build-perf.sh): runs
# SelfPlay -P on a position file alternating between the two exes (A B, B A, A B, ...), prints nodes, time,
# nodes/s and bytes allocated per node of every run, then the median ratio of the nodes/s (B / A) of the pairs
# with min and max. Both exes must search the same tree (identical node counts), otherwise the ratio means nothing.
#
# powershell -ExecutionPolicy Bypass -File speed_bench.ps1 -A SelfPlay-rel090p.exe -B SelfPlay-spdgp.exe `
#     -Fens tests.fen -Depth 10 -Pairs 5
param(
    [Parameter(Mandatory = $true)][string]$A,
    [Parameter(Mandatory = $true)][string]$B,
    [string]$Fens = "tests.fen",
    [int]$Depth = 10,
    [int]$Pairs = 5
)

$ErrorActionPreference = "Continue"

function Invoke-Perf([string]$exe) {
    $out = & $exe -P -i $Fens -d $Depth +RTS -s -RTS 2>&1 | ForEach-Object { "$_" } | Out-String
    $nodes = if ($out -match "Total nodes:\s+(\d+)") { [long]$Matches[1] } else { -1 }
    $ms    = if ($out -match "Total time \(ms\):\s+(\d+)") { [long]$Matches[1] } else { -1 }
    $alloc = if ($out -match "([\d,]+) bytes allocated in the heap") { [double]($Matches[1] -replace ",", "") } else { -1 }
    $nps = if ($ms -gt 0) { [math]::Round($nodes * 1000.0 / $ms) } else { 0 }
    $bpn = if ($nodes -gt 0) { [math]::Round($alloc / $nodes) } else { 0 }
    "{0,-28} nodes {1,10}  ms {2,7}  nodes/s {3,8}  bytes/node {4,6}" -f (Split-Path $exe -Leaf), $nodes, $ms, $nps, $bpn |
        Write-Host
    return [pscustomobject]@{ Nodes = $nodes; Nps = $nps; Bpn = $bpn }
}

function Get-Median([double[]]$xs) {
    $s = $xs | Sort-Object
    $n = $s.Count
    if ($n % 2 -eq 1) { return $s[($n - 1) / 2] }
    return ($s[$n / 2 - 1] + $s[$n / 2]) / 2
}

$A = (Resolve-Path $A).Path
$B = (Resolve-Path $B).Path
$ratios = @()
$nodesOk = $true
for ($i = 1; $i -le $Pairs; $i++) {
    Write-Host "Pair $i"
    if ($i % 2 -eq 1) { $ra = Invoke-Perf $A; $rb = Invoke-Perf $B }
    else              { $rb = Invoke-Perf $B; $ra = Invoke-Perf $A }
    if ($ra.Nodes -ne $rb.Nodes) { $nodesOk = $false }
    $ratios += $rb.Nps / $ra.Nps
}
$med = Get-Median $ratios
$min = ($ratios | Measure-Object -Minimum).Minimum
$max = ($ratios | Measure-Object -Maximum).Maximum
Write-Host ""
Write-Host ("Ratios B/A: " + (($ratios | ForEach-Object { "{0:N4}" -f $_ }) -join " "))
Write-Host ("Median ratio nodes/s B/A: {0:N4} (min {1:N4}, max {2:N4}) over {3} pairs" -f $med, $min, $max, $Pairs)
if (-not $nodesOk) { Write-Host "WARNING: node counts differ between A and B, the ratio is not a speed comparison" }
