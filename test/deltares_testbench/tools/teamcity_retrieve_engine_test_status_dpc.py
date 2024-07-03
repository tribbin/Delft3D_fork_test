import argparse
import getpass
import os
import shutil
import xml.etree.ElementTree as ET
from datetime import datetime

import requests
from requests.auth import HTTPBasicAuth

"""
Author: Jan Mooiman
E-Mail: jan.mooiman@deltares.nl
Date  : 10 sep 2017

This script list the test bench results with status: passed, failed, exception, ignored and muted.
The percentage is computed as follows: the passed tests divide by the total number of tests

The test benchroot need to specified by its projectid.
This can be taken from the web-adress.
Ex. DIMR testbench daily:  https://dpcbuild.deltares.nl/project.html?projectId=Delft3DSobek_DimrTestbench&tab=projectOverview
The project id is: Delft3DSobek_DimrTestbench
See the examples below
Structure of testbench should be: root -> engine test -> functionality tests

teamcity_retrieve_engine_test_status.py --tbroot DFlowFlexibleMesh
teamcity_retrieve_engine_test_status.py --tbroot Dimr_DimrTestbenchRelease  # DIMR testbench release
teamcity_retrieve_engine_test_status.py --tbroot Delft3DSobek_DimrTestbench  # DIMR testbench daily
"""
summarydata_array = []
global log_file
global engine_statistics


class summarydata(object):
    def __init__(self, name):
        self.name = name
        self.sum_passed = 0
        self.sum_failed = 0
        self.sum_exception = 0
        self.sum_ignored = 0
        self.sum_muted = 0


class Data(object):
    def __init__(self, name, passed, failed):
        self.name = name
        self.passed = passed
        self.failed = failed
        self.total = passed + failed
        a = 0.0
        if self.total > 0:
            a = float(self.passed) / float(self.total) * 100.0
        self.percentage = a


def lprint(*args, **kwargs):
    global log_file
    log_file.write(" ".join(map(str, args)) + "\n")


def report_cases(url, given_build_config, username, password, buildname):
    _sum_passed = 0
    _sum_failed = 0
    _sum_exception = 0
    _sum_ignored = 0
    _sum_muted = 0

    global _enginge_statistics
    global summarydata_array

    case_id = []
    case_name = []
    engine_req = requests.get(
        url=url, auth=HTTPBasicAuth(username, password), stream=True, verify=True
    )
    try:
        xml_engine_root = ET.fromstring(engine_req.text)
    except:
        print("Text is not in XML format: %s" % engine_req.text)
        return 1

    for build_types in xml_engine_root.findall("buildTypes"):
        for build_type in build_types:
            if len(given_build_config) != 0:
                for i in range(len(given_build_config)):
                    if given_build_config[i] == build_type.attrib["id"]:
                        case_id.append(build_type.attrib["id"])
                        case_name.append(build_type.attrib["name"])
            else:
                case_id.append(build_type.attrib["id"])
                case_name.append(build_type.attrib["name"])

    if len(case_id) != 0:
        print("        %s" % xml_engine_root.attrib["name"])
        lprint("        %s" % xml_engine_root.attrib["name"])
        lprint(
            "               total   passed   failed   except  ignored    muted        %  --- test case name            (# build)"
        )

    # print case_list
    build_nr = []
    passed = []
    failed = []
    exception = []
    ignored = []
    muted = []
    muted_exception = []

    sum_passed_subtotal = 0
    not_passed_subtotal = 0

    for case in case_id:
        computation_name = []
        deltares_build = "https://dpcbuild.deltares.nl"
        url = (
            "%s/httpAuth/app/rest/builds?locator=buildType:(id:%s),defaultFilter:false,branch:<default>&count=1&fields=count,build(number,statistics,status,statusText,testOccurrences,agent,lastChange,tags(tag),pinned,revisions(revision))"
            % (deltares_build, case)
        )
        case_req = requests.get(
            url=url, auth=HTTPBasicAuth(username, password), stream=True, verify=True
        )
        try:
            xml_case_root = ET.fromstring(case_req.text)
        except:
            print("Text is not in XML format: %s" % case_req.text)
            return 1

        file_name = "TMPdownload_teamcity_retrieve/%s.xml" % case
        with open(file_name, "wb") as out_file:
            out_file.write(case_req.content)

        for build in xml_case_root.findall("build"):
            bnr = build.attrib["number"]
            build_nr.append(bnr)
            status_text = ""

            if build.find("./testOccurrences") is not None:
                if "passed" in build.find("./testOccurrences").attrib:
                    passed.append(int(build.find("./testOccurrences").attrib["passed"]))
                else:
                    passed.append(0)
                if "failed" in build.find("./testOccurrences").attrib:
                    failed.append(int(build.find("./testOccurrences").attrib["failed"]))
                else:
                    failed.append(0)
                if "ignored" in build.find("./testOccurrences").attrib:
                    ignored.append(
                        int(build.find("./testOccurrences").attrib["ignored"])
                    )
                else:
                    ignored.append(0)
                if "muted" in build.find("./testOccurrences").attrib:
                    muted.append(int(build.find("./testOccurrences").attrib["muted"]))
                else:
                    muted.append(0)
            else:
                passed.append(0)
                failed.append(0)
                ignored.append(0)
                muted.append(0)
                status = build.find("statusText")
                if status is not None:
                    status_text = status.text
                else:
                    status_text = "Build failed!"

        exception.append(0)  # first guess, initially there are no exceptions
        # first guess, initially there are no exceptions for a muted test
        muted_exception.append(0)

        if len(failed) == 0:
            lprint("ERROR: No data available for project %s" % case)
            continue

        i = build_nr.__len__() - 1
        a = 0
        b = 0
        if failed[i] != 0:
            cnt = int(build.find("./testOccurrences").attrib["count"])
            href = build.find("./testOccurrences").attrib["href"]
            url_1 = "%s%s,count:%d" % (deltares_build, href, cnt)
            test_occs_req = requests.get(
                url=url_1,
                auth=HTTPBasicAuth(username, password),
                stream=True,
                verify=True,
            )
            try:
                xml_test_occs = ET.fromstring(test_occs_req.text)
            except:
                print("Text is not in XML format: %s" % test_occs_req.text)
                return 1
            for t_occ in xml_test_occs.findall("testOccurrence"):
                if t_occ.attrib["status"] == "FAILURE":
                    href = t_occ.attrib["href"]
                    url_2 = "%s%s" % (deltares_build, href)
                    test_occ_req = requests.get(
                        url=url_2,
                        auth=HTTPBasicAuth(username, password),
                        stream=True,
                        verify=True,
                    )
                    try:
                        xml_test_occ = ET.fromstring(test_occ_req.text)
                    except:
                        print("Text is not in XML format: %s" % test_occ_req.text)
                        return 1
                    txt = xml_test_occ.find("details").text

                    try:
                        if (
                            txt.find("Exception occurred") != -1
                            or txt.find("exception occurred") != -1
                        ):
                            if "muted" in t_occ.attrib:
                                exception[i] += 1
                                muted_exception[i] += 1
                                computation_name.append(
                                    "MUTED: " + xml_test_occ.attrib["name"]
                                )
                            else:
                                failed[i] -= 1
                                exception[i] += 1
                                computation_name.append(xml_test_occ.attrib["name"])
                    except:
                        error_message = "ERROR retrieving data from last build for {case_name} : {xml_attrib}.".format(
                            case_name=case_name[i],
                            xml_attrib=xml_test_occ.attrib["name"],
                        )
                        print(error_message)
                        lprint(error_message)

        total = (
            passed[i]
            + failed[i]
            + exception[i]
            + ignored[i]
            + muted[i]
            - muted_exception[i]
        )
        if total != 0:
            a = float(passed[i]) / float(total) * 100.0
        else:
            a = 0

        if total > 0:
            lprint(
                "            %8d %8d %8d %8d %8d %8d %8.2f  ---  %-24s (#%s)"
                % (
                    total,
                    passed[i],
                    failed[i],
                    exception[i],
                    ignored[i],
                    muted[i],
                    a,
                    case_name[i],
                    build_nr[i],
                )
            )
        else:
            lprint(
                "                   x        x        x        x        x        x        x  ---  %-24s (#%s)"
                % (case_name[i], build_nr[i])
            )
            lprint(
                "                                                                            xxx  %s"
                % (status_text)
            )

        if exception[i] != 0:
            for j in range(0, computation_name.__len__()):
                lprint(
                    "                                                                            xxx  Exception %s"
                    % computation_name[j]
                )

        _sum_passed += passed[i]
        _sum_failed += failed[i]
        _sum_exception += exception[i]
        _sum_ignored += ignored[i]
        _sum_muted += muted[i]
        sum_passed_subtotal += passed[i]
        not_passed_subtotal += failed[i] + exception[i] + ignored[i] + muted[i]

    for summary in summarydata_array:
        if (summary.name in buildname) or summary.name == "All":
            summary.sum_passed += _sum_passed
            summary.sum_failed += _sum_failed
            summary.sum_exception += _sum_exception
            summary.sum_ignored += _sum_ignored
            summary.sum_muted += _sum_muted

    if len(case_id) != 0:
        engine_statistics.append(
            Data(
                xml_engine_root.attrib["name"], sum_passed_subtotal, not_passed_subtotal
            )
        )

        i = len(engine_statistics) - 1
        lprint("            Total     : %6d" % engine_statistics[i].total)
        lprint("            Passed    : %6d" % engine_statistics[i].passed)
        lprint("            Percentage: %6.2f" % engine_statistics[i].percentage)


def main(tbroot, given_build_config, username, password, engines):
    global engine_statistics
    global summarydata_array

    summarydata_array.append(summarydata("All"))
    if engines is not None:
        for engine in engines.split(","):
            summarydata_array.append(summarydata(engine))

    urltb = "https://dpcbuild.deltares.nl/httpAuth/app/rest/projects/id:%s" % tbroot

    try:
        tbroot_req = requests.get(
            url=urltb, auth=HTTPBasicAuth(username, password), stream=True, verify=True
        )
    except:
        print("Given URL does not exist: %s" % urltb)
        return 1

    try:
        xml_tb_root = ET.fromstring(tbroot_req.text)
    except:
        print("Text is not in XML format: %s" % tbroot_req.text)
        return 1

    print("")
    print("%s" % xml_tb_root.attrib["name"])
    lprint("%s" % xml_tb_root.attrib["name"])

    engine_name = []
    engine_id = []
    for projects in xml_tb_root.findall("projects"):
        for project in projects:
            engine_id.append(project.attrib["id"])
            engine_name.append(project.attrib["name"])

    engine_statistics = []

    for engine in engine_id:
        project_id = []
        project_name = []

        url = "https://dpcbuild.deltares.nl/httpAuth/app/rest/projects/id:%s" % engine

        engine_req = requests.get(
            url=url, auth=HTTPBasicAuth(username, password), stream=True, verify=True
        )
        try:
            xml_engine_root = ET.fromstring(engine_req.text)
        except:
            print("Text is not in XML format: %s" % engine_req.text)
            return 1
        print("    %s" % xml_engine_root.attrib["name"])
        lprint("    %s" % xml_engine_root.attrib["name"])

        for projects in xml_engine_root.findall("projects"):
            for project in projects:
                project_id.append(project.attrib["id"])
                project_name.append(project.attrib["name"])

                url_3 = (
                    "https://dpcbuild.deltares.nl/httpAuth/app/rest/projects/id:%s"
                    % project.attrib["id"]
                )
                level_req = requests.get(
                    url=url,
                    auth=HTTPBasicAuth(username, password),
                    stream=True,
                    verify=True,
                )
                try:
                    level_root = ET.fromstring(level_req.text)
                except:
                    print("Text is not in XML format: %s" % level_req.text)
                    return 1
                report_cases(
                    url_3,
                    given_build_config,
                    username,
                    password,
                    project.attrib["name"],
                )

        report_cases(
            url,
            given_build_config,
            username,
            password,
            engine_name[engine_id.index(engine)],
        )

    lprint("\nTestbench root: %s" % xml_tb_root.attrib["name"])
    for summary in summarydata_array:
        total = (
            summary.sum_passed
            + summary.sum_failed
            + summary.sum_exception
            + summary.sum_ignored
            + summary.sum_muted
        )
        not_passed = (
            summary.sum_failed + summary.sum_exception + summary.sum_ignored + summary.sum_muted
        )
        a = 0.0
        if total > 0:
            a = float(summary.sum_passed) / float(total) * 100.0

        lprint("\nSummary: %s" % summary.name)
        lprint("Total tests   : %6d" % (total))
        lprint("    Passed    : %6d" % summary.sum_passed)
        lprint("    Not passed: %6d" % not_passed)
        lprint("    Failed    : %6d" % summary.sum_failed)
        lprint("    Exception : %6d" % summary.sum_exception)
        lprint("    Ignored   : %6d" % summary.sum_ignored)
        lprint("    Muted     : %6d" % summary.sum_muted)
        lprint("    Percentage: %6.2f" % float(a))


def create_argument_parser():
    parser = argparse.ArgumentParser(
        description="Retrieve status of a testbench running on TeamCity"
    )

    parser.add_argument(
        "-t",
        "--tbroot",
        help="ProjetcId of the testbench root for which the status is needed.",
        dest="tbroot",
        required="true",
    )
    parser.add_argument("-o", "--output", help="Output filename.", dest="out_put")
    parser.add_argument(
        "-b", "--build_config", help="Build configuration ID", dest="build_config"
    )
    parser.add_argument(
        "-u", "--username", help="Username for accessing TeamCity.", dest="username"
    )
    parser.add_argument(
        "-p",
        "--password",
        help="Password belonging to username for accessing TeamCity.",
        dest="password",
    )
    parser.add_argument(
        "-i",
        "--interactive",
        help="Must be True to enable username/password via keyboard.",
        dest="interactive",
    )
    parser.add_argument(
        "-e",
        "--engines",
        help="Specify extra components to be summarized, between double quotes and separated by a komma",
        dest="engines",
    )

    return parser


if __name__ == "__main__":
    start_time = datetime.now()

    if os.path.exists("TMPdownload_teamcity_retrieve"):
        shutil.rmtree("TMPdownload_teamcity_retrieve")
    os.mkdir("TMPdownload_teamcity_retrieve")

    parser = create_argument_parser()
    args = parser.parse_args()

    out_put = "teamcity_retrieve_engine_test_status.txt"
    given_build_config = []

    if args.tbroot:
        tbroot = args.tbroot
    if args.build_config:
        bconfig = args.build_config
        given_build_config = bconfig.split(",")
    if args.out_put:
        out_put = args.out_put
    if args.interactive:
        interactive = args.interactive
    else:
        interactive = False
    if args.username:
        username = args.username
    else:
        if interactive:
            username = input("Username for TeamCity access:")
        else:
            print(
                'No username on commandline. add "-i True" to enable interactive input'
            )
            exit()
    if args.password:
        password = args.password
    else:
        if interactive:
            password = getpass.getpass()
        else:
            print(
                'No password on commandline. add "-i True" to enable interactive input'
            )
            exit()
    if args.engines:
        engines = args.engines
    else:
        engines = None
    if os.path.exists(out_put):
        os.remove(out_put)
    log_file = open(out_put, "a")

    print("Start: %s\n" % start_time)
    lprint("Start: %s\n" % start_time)

    print("Listing is written to: %s" % out_put)

    main(tbroot, given_build_config, username, password, engines)

    if os.path.exists("TMPdownload_teamcity_retrieve"):
        shutil.rmtree("TMPdownload_teamcity_retrieve")

    lprint("\nStart: %s" % start_time)
    lprint("End  : %s" % datetime.now())
    lprint("Ready")
    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now())
    print("Ready")
