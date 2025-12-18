$sourceDir = $env:TEMP
$destDir = "C:\Users\mindy\Downloads\outlook_talk"
$minTime = (Get-Date).AddMinutes(-15)

# Get potential files (recursive, entering subfolders which Playwright uses)
$files = Get-ChildItem -Path $sourceDir -Recurse -File -ErrorAction SilentlyContinue | 
         Where-Object { $_.LastWriteTime -gt $minTime -and $_.Length -gt 100000 }

Write-Host "Found $($files.Count) potential files."

foreach ($file in $files) {
    try {
        # Read the first few lines to check content
        $content = Get-Content -Path $file.FullName -TotalCount 5 -ErrorAction Stop
        
        # Check for WASDE header signature
        if ($content[0] -match '"WasdeNumber","ReportDate"') {
            # Extract date from second line (data row)
            # Row 2: "608","January 2021","...
            $columns = $content[1] -split '","'
            # $columns[1] should be "January 2021" (after stripping quotes if needed)
            $dateStr = $columns[1].Trim('"') 
            
            # Parse date
            try {
                $dateObj = [datetime]::ParseExact($dateStr, "MMMM yyyy", $null)
                $formattedDate = $dateObj.ToString("yyyy-MM")
                
                $newFileName = "oce-wasde-report-data-$formattedDate.csv"
                $destPath = Join-Path -Path $destDir -ChildPath $newFileName
                
                # Check for existing to avoid overwrite/duplication if not needed (or force if needed)
                if (-not (Test-Path $destPath)) {
                    Copy-Item -Path $file.FullName -Destination $destPath -Force
                    Write-Host "Recovered: $formattedDate -> $newFileName"
                } else {
                    Write-Host "Skipping $formattedDate (Already exists)"
                }
            } catch {
                Write-Host "Could not parse date '$dateStr' in file $($file.Name)"
            }
        }
    } catch {
        # Ignore read errors (locked files etc)
    }
}

# Final count
$finalCount = (Get-ChildItem -Path $destDir -Filter "oce-wasde-report-data-*.csv").Count
Write-Host "Total WASDE CSVs in project folder: $finalCount"
