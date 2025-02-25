import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object Sign : BuildType({

    name = "Sign"
    buildNumberPattern = "%build.vcs.number%"
    artifactRules = "to_sign => oss_artifacts_x64_%build.vcs.number%.zip!x64"

    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        powerShell {
            name = "Remove already signed binaries"
            platform = PowerShellStep.Platform.x64
            workingDir = "to_sign"
            scriptMode = script {
                content = """
                    ${'$'}files = Get-ChildItem -Path . -Recurse -Include *.exe,*.dll | Where-Object { -not ${'$'}_.PSIsContainer }
                    
                    foreach (${'$'}file in ${'$'}files) {
                        try {
                            # Check if the file is signed
                            ${'$'}signature = Get-AuthenticodeSignature -FilePath ${'$'}file.FullName
                            if (${'$'}signature.Status -eq 'Valid') {
                                # Remove the signed file
                                Remove-Item -Path ${'$'}file.FullName -Force
                                Write-Output "Removed signed file: ${'$'}(${'$'}file.FullName)"
                            }
                        } catch {
                            Write-Output "Error processing file: ${'$'}(${'$'}file.FullName)"
                        }
                    }
                """.trimIndent()
            }
        }        
        powerShell {
            name = "Remove tclkitsh852.exe"
            platform = PowerShellStep.Platform.x64
            workingDir = "to_sign"
            scriptMode = script {
                // We cannot sing this binary, see: https://wiki.tcl-lang.org/page/SDX+under+Windows
                content = """Remove-Item -Path bin\\tclkitsh852.exe -Force""".trimIndent()
            }
        }
        powerShell {
            name = "Sign"
            platform = PowerShellStep.Platform.x64
            workingDir = "to_sign"
            scriptMode = script {
                content = """
                    Get-ChildItem -Path . -Recurse -Include *.exe,*.dll | Foreach { signtool sign /v /debug /fd SHA256 /tr "http://timestamp.acs.microsoft.com" /td SHA256 /dlib "C:\ProgramData\Microsoft\MicrosoftTrustedSigningClientTools\Azure.CodeSigning.Dlib.dll" /dmdf "C:\ProgramData\Microsoft\MicrosoftTrustedSigningClientTools\metadata.json" ${'$'}_.fullname }
                """.trimIndent()
            }
        }
    }

    dependencies {
        dependency(AbsoluteId("${DslContext.getParameter("delft3d_project_root")}_WindowsBuild")) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
            artifacts {
                cleanDestination = true
                artifactRules = """
                    ?:*_x64_*.zip!/x64/**/*.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/*.dll => to_sign
                """.trimIndent()
            }
        }
    }

    features {
        if (DslContext.getParameter("environment") == "production") {
            commitStatusPublisher {
                enabled = true
                vcsRootExtId = "${DslContext.settingsRoot.id}"
                publisher = gitlab {
                    authType = vcsRoot()
                }
            }
        }
    }

})
