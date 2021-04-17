Param([Parameter(Mandatory=$true)]$Version, $VersionSuffix)

$Version = [Version]::Parse($Version)
$manifestVersion = $Version.ToString(3)

$packageVersion = $VersionSuffix ? "$manifestVersion-$VersionSuffix" : $manifestVersion

push-location $psScriptRoot

try {
    # Delete any existing VSIX files from this directory
    Remove-Item *.vsix -Verbose

    # Set the version in the VSIX manifest
    $vsixManifest = Join-Path $psScriptRoot "Hally.Alignment.VisualStudio/source.extension.vsixmanifest"
    $xml = [xml]$(Get-Content $vsixManifest)
    $xml.PackageManifest.Metadata.Identity.Version = $manifestVersion
    $xml.Save($vsixManifest)

    # Set the version of the VS assembly (it's an old-style format project)
    $assemblyInfo = "Hally.Alignment.VisualStudio/Properties/AssemblyInfo.cs"

    $updated =
        Get-Content $assemblyInfo `
        | ForEach-Object {$_ -replace "Version\(`".*$", "Version(`"$Version`")]"}
    $updated | Set-Content -Encoding utf8 $assemblyInfo

    # Build everything, passing in the version for the SDK-style projects
    msbuild /nologo VisualStudioAlignment.sln /p:Configuration=Release /v:minimal /p:Version=$Version /m

    # Move the VSIX to this script's directory, adding the short version string
    Move-Item `
        Hally.Alignment.VisualStudio\bin\Release\Hally.Alignment.VisualStudio.vsix `
        Hally.Alignment.VisualStudio.$packageVersion.vsix

    # Dump a few files to show things have been updated
    Get-ChildItem .\Hally.Alignment.VisualStudio\bin\Release\Hally.*.dll, *.vsix
}
finally {
    Pop-Location
}
