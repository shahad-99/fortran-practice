function Get-VSCodeActiveFile {
    [CmdletBinding()]
    param()

    try {
        # Get the main Visual Studio Code process.
        # It's named 'Code' and will have a MainWindowTitle.
        $vsCodeProcess = Get-Process | Where-Object { $_.ProcessName -eq 'Code' -and $_.MainWindowTitle }

        if ($vsCodeProcess) {
            # In case multiple VS Code windows are open, we'll get the one that was last active.
            # We can't know for sure which one is "active" from PowerShell's perspective,
            # so we'll just take the first one found. For most users, this is sufficient.
            $windowTitle = $vsCodeProcess[0].MainWindowTitle

            # The title format is usually "FileName.ext - WorkSpaceName - Visual Studio Code"
            # We split the string by the hyphen character.
            $titleParts = $windowTitle.Split(' - ')

            if ($titleParts.Count -ge 3) {
                # The file name is the first part. We trim any whitespace.
                $fileName = $titleParts[0].Trim()
                
                # The workspace path is usually part of the title.
                # We can construct a full path if the workspace is known.
                $workSpaceName = $titleParts[1].Trim()

                # Get the full path by combining the workspace folder with the file name.
                # Note: This assumes the file is in the root of the workspace.
                # To get the true full path, more advanced methods are needed (see below).
                $fullPath = Join-Path -Path (Get-Location) -ChildPath $fileName # This is a guess

                $result = [PSCustomObject]@{
                    FileName      = $fileName
                    Workspace     = $workSpaceName
                    WindowTitle   = $windowTitle
                    GuessedFullPath = $fullPath # Note: This is not always accurate
                }

                return $result
            }
            else {
                Write-Warning "Could not parse the VS Code window title: '$windowTitle'"
                return $null
            }
        }
        else {
            Write-Warning "Visual Studio Code process not found or no window is open."
            return $null
        }
    }
    catch {
        Write-Error "An error occurred: $_"
        return $null
    }
}

# Prerequisites: Ensure the following functions from our previous conversations are loaded
# in your PowerShell session or profile:
# 1. Get-VSCodeActiveFile
# 2. Get-FileInfo

# --- Step 1: Get the active file information from VS Code ---
$loc = Get-VSCodeActiveFile

# Add a check to ensure VS Code was found and a file is active
if (-not $loc) {
    Write-Warning "Could not determine the active file in VS Code. Aborting script."
    return # Stop the script
}

$file = $loc.FileName 
$info = Get-FileInfo $file 

# Add a check to ensure the file was found on disk
if (-not $info) {
    Write-Warning "File '$file' not found in the current directory. Aborting script."
    return # Stop the script
}

$basename = $info.BaseName 
$name = $info.Name
$executableName = "$($basename).exe"

# --- Step 2: Compile the Fortran code ---
Write-Host "Attempting to compile '$name'..." -ForegroundColor Yellow
gfortran $name -o $basename

# --- Step 3: Check if compilation was successful ---
if ($LASTEXITCODE -eq 0) {
    # If the exit code is 0, compilation succeeded
    Write-Host "Compilation successful. Running '$executableName'..." -ForegroundColor Green
    
    # --- Step 4: Run the compiled executable ---
    # Use the call operator '&' to execute the program.
    # The '.\' is crucial to specify that the file is in the current directory.
    & ".\$executableName"
}
else {
    # If the exit code is not 0, there was an error
    Write-Error "Compilation failed. Please check the output from gfortran for errors."
}