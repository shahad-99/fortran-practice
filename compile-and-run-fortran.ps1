# C:\Scripts\Compile-And-Run-Fortran.ps1

# Ensure the helper functions are available.
# It's best practice to define them right in the script that uses them.
function Get-FileInfo {
    [CmdletBinding()]
    param([Parameter(Mandatory=$true, ValueFromPipeline=$true)][string]$FilePath)
    process {
        try {
            $file = Get-Item -Path $FilePath -ErrorAction Stop
            [PSCustomObject]@{
                FullName  = $file.FullName
                Name      = $file.Name
                BaseName  = $file.BaseName
                Extension = $file.Extension
            }
        } catch { Write-Error "Failed to get file info for '$FilePath'. Error: $_" }
    }
}

function Get-VSCodeActiveFile {
    [CmdletBinding()]
    param()
    try {
        $vsCodeProcess = Get-Process | Where-Object { $_.ProcessName -eq 'Code' -and $_.MainWindowTitle }
        if ($vsCodeProcess) {
            $windowTitle = $vsCodeProcess[0].MainWindowTitle
            $titleParts = $windowTitle.Split(' - ')
            if ($titleParts.Count -ge 3) {
                [PSCustomObject]@{
                    FileName  = $titleParts[0].Trim()
                    Workspace = $titleParts[1].Trim()
                }
            }
        }
    } catch { # Fails silently if VS Code isn't running }
}


# --- Main script logic ---
$loc = Get-VSCodeActiveFile
if (-not $loc) {
    Write-Warning "Could not find an active file in VS Code."
    return
}

$info = Get-FileInfo $loc.FileName
if (-not $info) {
    Write-Warning "File '$($loc.FileName)' not found in the current directory."
    return
}
}

$basename = $info.BaseName
$name = $info.Name
$executableName = "$($basename).exe"

Write-Host "Compiling '$name'..." -ForegroundColor Yellow
gfortran $name -o $basename

if ($LASTEXITCODE -eq 0) {
    Write-Host "Compilation successful. Running '$executableName'..." -ForegroundColor Green
    & ".\$executableName"
} else {
    Write-Error "Compilation failed."
}