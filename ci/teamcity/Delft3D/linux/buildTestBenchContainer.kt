package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.step.*

object LinuxBuildTestbenchContainer : BuildType({

    templates(
        TemplateMergeRequest,
    )

    id("LinuxTestbenchContainer")
    name = "Build Testbench Container"
    description = "Create a container with the testbench (also for H7)"

    artifactRules = "container.txt"
    buildNumberPattern = "%build.vcs.number%"

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        python {
            name = "Check merge conflicts"
            id = "Check_merge_conflicts"

            conditions {
                contains("teamcity.build.branch", "merge-requests")
            }
            command = script {
                content = """
                    import subprocess
                    import json
                    import time
                    
                    merge_branch = "%teamcity.build.branch%".replace("merge-requests", "merge_requests")
                    for i in range(5):
                        time.sleep(60)
                        url = f"https://git.deltares.nl/api/v4/projects/14/{merge_branch}"
                        merge_request_details = subprocess.check_output(["curl", "--header", f"PRIVATE-TOKEN: %gitlab_private_access_token%", url])
                        merge_request_data = json.loads(merge_request_details)
                        merge_status = merge_request_data.get("merge_status", "")
                        if merge_status == "can_be_merged":
                            print("##teamcity[message text='No merge conflicts']")
                            break
                        elif i == 4:
                            print("##teamcity[buildProblem status='FAILURE' description='Could not verify merge_status of merge-request.']")
                            break
                        else:
                            has_conflicts = merge_request_data.get("has_conflicts", "")
                            if has_conflicts:
                                print("##teamcity[buildProblem status='FAILURE' description='Merge conflicts prevented run']")
                                break
                        print(f"##teamcity[message text='Retry get merge_status from gitlab #{str(i+1)}']")
                """.trimIndent()
            }
        }
        mergeTargetBranch {}
        dockerCommand {
            name = "Build docker testbench container"
            id = "Generate_docker_testbench_container"
            commandType = build {
                source = file {
                    path = "test/deltares_testbench/ci/dockerfiles/testbench.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%"
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Push docker testbench container"
            id = "Push_docker_testbench_container"
            commandType = push {
                namesAndTags = "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%"
            }
        }
        script {
            name = "Create artifact"
            id = "Create_artifact"
            scriptContent = """
                file="container.txt"
                echo "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%" > ${'$'}file
            """.trimIndent()
        }
    }

    features {
        dockerSupport {
            cleanupPushedImages = true
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D_DEV"
            }
        }
    }
})