package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.template.*
import Delft3D.step.*


object TestPythonCiTools : BuildType({
    id("TestPythonCiTools")
    name = "Test Python CI tools and DIMRset Delivery"
    description = """
        Runs tests and quality checks on the python CI tools and DIMRset delivery scripts.
    """.trimIndent()

    // The name `coverage.zip` for the pytest coverage report should not be changed.
    // Using the name `coverage.zip` will ensure TeamCity adds the `Coverage` tab to the build.
    // See: https://www.jetbrains.com/help/teamcity/importing-arbitrary-coverage-results-to-teamcity.html
    artifactRules = """
        +:ci/python/*.xml => report
        +:ci/python/htmlcov/* => coverage.zip
        +:ci/DIMRset_delivery/*.xml => report
        +:ci/DIMRset_delivery/htmlcov/* => coverage_dimrset.zip
    """.trimIndent()

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest
    )

    vcs {
        root(DslContext.settingsRoot)
        excludeDefaultBranchChanges = true  // Only include changes made within the branch of this build.
        cleanCheckout = true
    }

    triggers {
        vcs { 
            // Trigger this build only if there are changes to the files matching these rules.
            // Absolute paths match paths relative to the VCS root.
            // See: https://www.jetbrains.com/help/teamcity/configuring-vcs-triggers.html#General+Syntax
            triggerRules = """
                +:/ci/python/**/*.py
                +:/ci/python/pyproject.toml
                +:/ci/python/uv.lock
                +:/ci/DIMRset_delivery/**/*.py
                +:/ci/DIMRset_delivery/requirements.txt
                +:/ci/DIMRset_delivery/pytest.ini
            """.trimIndent()
            branchFilter = "+:pull/*"
        }
    }

    steps {
        mergeTargetBranch {}
        python {
            name = "Check code formatting"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "ruff"
                scriptArguments = "format --diff"
            }
        }
        python {
            name = "Run linter"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "ruff"
                scriptArguments = "check --output-format=junit --output-file=ruff.xml"
            }
        }
        python {
            name = "Run type checker"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "mypy"
                scriptArguments = "ci_tools --junit-xml=mypy.xml"
            }
        }
        python {
            name = "Run unit tests"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "pytest"
                scriptArguments = """
                    --junitxml=pytest.xml
                    --cov-report=html
                    --cov=.
                """.trimIndent()
            }
        }
        python {
            name = "Run DIMRset delivery linter"
            workingDir = "ci/DIMRset_delivery"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = "requirements.txt"
            }
            command = module {
                module = "ruff"
                scriptArguments = "check src/ --output-format=junit --output-file=ruff_dimrset.xml"
            }
        }
        python {
            name = "Run DIMRset delivery tests"
            workingDir = "ci/DIMRset_delivery"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = "requirements.txt"
            }
            command = module {
                module = "pytest"
                scriptArguments = """
                    test/
                    --junitxml=pytest_dimrset.xml
                    --cov-report=html
                    --cov=src
                    --verbose
                """.trimIndent()
            }
        }
    }

    features {
        xmlReport {
            reportType = XmlReport.XmlReportType.JUNIT
            rules = """
                +:ci/python/ruff.xml
                +:ci/python/mypy.xml
                +:ci/python/pytest.xml
                +:ci/DIMRset_delivery/ruff_dimrset.xml
                +:ci/DIMRset_delivery/pytest_dimrset.xml
            """.trimIndent()
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})