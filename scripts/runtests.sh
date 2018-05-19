#!/bin/bash

BINDIR=${PWD}/../build/bin
TESTDIR=${BINDIR}/tests

passcount=0
failcount=0

${TESTDIR}/test_testcase_init
${TESTDIR}/test_testcase_reset
