#!/bin/env python
'''Python script for collecting test results of kgen kernels in "https://github.com/NCAR/kernelOptimization" Github repo.

    Author: Youngsung Kim (youngsun@ucar.edu)

    History:
      - Created at May 1, 2016

    KGen Kernel Execution Result File Format( Revision 0.1 ):
      - Test data is saved in JSON format
      - Accepted primitive test data formats are boolean, string, integer, and floating-pointer number.
      - Accepted container data formats are list and dictionary.
      - String is saved in unicode character format.
      - Mandated test items are:
        * "cpu_model": cpu model name of a test system - string
        * "total_memsize": total memory of a test system - string
        * "uname": uname result of a test system - string
        * "compiler": compiler version being used - string
        * "cases": this is a dictionary that contains results of each test cases - dictionary.
            ** <testname>: Each test result much contain at least following items. - dictionary in the form of <testname> : <dictionary>
                *** "begin": test start date and time (YYYY-MM-DD HH:MM:SS.SSSSSS) - string
                *** "end": test end date and time - string
                *** "passed": verification result - boolean
                *** "tolerance": tolerance for verification - float
                *** "difference": Verification difference - list of float numbers
                *** "diff_type": The type of difference and tolerance  0: Normalized RMS difference
                *** "elapsed_time": Elapsed time in micro-second for a test case list of float numbers
      - Other test information can be added in similar way to the mandated information.
'''

from __future__ import print_function
import os
import sys
import json
import datetime
import subprocess

NREPEAT = 3
SCRIPT_HOME, SCRIPT_NAME = os.path.split(os.path.realpath(__file__))
MAKEFILE = 'Makefile.pgi'
SIGNATURE = '# Makefile for KGEN-generated kernel\n'

def run_shcmd(cmd, input=None, stderr_exit=True, **kwargs):
    ''' executing shell command. The last white spaces will be removed'''

    try:
        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
        out, err = proc.communicate(input=input)
        if stderr_exit and err:
            print ('COMMAND ERROR: %s\n\n'%cmd, str(err))
            sys.exit(-1)

        if out is None: out = ''
        if err is None: err = ''
        return out.rstrip(), err.rstrip()

    except Exception as e:
        print ('SHELL ERROR: %s\n\n'%cmd, str(e))
        sys.exit(-1)

def main():
    ''' Main function: generated test data through executing linux commands and kgen kernels'''

    if len(sys.argv) > 1:
        testroots = sys.argv[1:]
    else:
        print ('Please specify root paths to be tested.')
        print ('Usage %s [ testroot [ testroot ... ] ]'%sys.argv[0])
        sys.exit(-1)
        
    tests = {}

    # madatory information
    out, err = run_shcmd('cat /proc/cpuinfo | grep "model name"')
    tests['cpu_model'] = out

    out, err = run_shcmd('cat /proc/meminfo | grep -i memtotal')
    tests['total_memsize'] = out

    out, err = run_shcmd('uname -a')
    tests['uname'] = out

    out, err = run_shcmd('pgfortran --version', stderr_exit=False)
    tests['compiler'] = out.strip()

    # optional information such as:

    out, err = run_shcmd('git rev-parse --abbrev-ref HEAD')
    tests['git_branch'] = out

    out, err = run_shcmd('git rev-parse HEAD')
    tests['git_commit'] = out

    #out, err = run_shcmd('env')
    #tests['env'] = out

    #out, err = run_shcmd('top -n1 -b')
    #tests['top'] = out

    #out, err = run_shcmd('git diff')
    #tests['gitdiff'] = out


    ######### START OF TEST DATA COLLECTION ###########
    tests['begin'] = str(datetime.datetime.now())

    tests['cases'] = {}

    for testroot in testroots:
        for dirName, subdirList, fileList in os.walk(testroot):
            relpath = os.path.relpath(dirName, testroot)
            if relpath==SCRIPT_HOME.split('/')[-1]: del subdirList[:]
            if MAKEFILE not in fileList: continue

            makefile = []
            with open(os.path.join(dirName, MAKEFILE), 'r') as f:
                makefile = f.readlines()
                if len(makefile)<4: continue
                if makefile[0]!=SIGNATURE: continue

            print('***************************************************************')
            print('      TEST for ', dirName )
            print('***************************************************************')

            try:
                summary = {'elapsed_time': [], 'difference': [], 'diff_type': 0, 'tolerance': 0.0, 'passed': False, 'begin': str(datetime.datetime.now()), 'end': 'Not completed'}
                tests['cases'][dirName] = summary

                # initialize test
                out, err = run_shcmd('make -f %s clean'%MAKEFILE, stderr_exit=False, cwd=dirName)

                # run test
                out = ''
                for _ in range(NREPEAT):
                    o, e= run_shcmd('make -f %s'%MAKEFILE, stderr_exit=False, cwd=dirName)
                    out = '%s\n%s'%(out, o)

                summary['end'] = str(datetime.datetime.now())

                npassed = 0
                nfailed = 0
                nexttime = False
                nextdiff = False
                nexttol = False

                for outline in out.split('\n'):
                    if outline.find('FAILED')>=0:
                        nfailed += 1
                    elif outline.find('PASSED')>=0:
                        npassed += 1

                    if nexttime or outline.find('time')>=0 or outline.find('Time')>=0:
                        try:
                            etime = float(outline.split()[-1])
                            if outline.find('usec')>=0 and outline.find('summary')<0:
                                summary['elapsed_time'].append(float(etime))
                            elif outline.find('sec')>=0 and outline.find('summary')<0:
                                summary['elapsed_time'].append(float(etime)*1000000.0)
                            if nexttime: nexttime = False
                        except:
                            nexttime = not nexttime
                            
                    if nextdiff or outline.find('Normalized RMS of difference')>=0:
                        try:
                            diff = float(outline.split()[-1])
                            summary['difference'].append(float(diff))
                            if nextdiff: nextdiff = False
                        except:
                            nextdiff = not nextdiff

                    if nexttol or outline.find('tolerance')>=0 or outline.find('Tolerance')>=0:
                        try:
                            tol = float(outline.split()[-1])
                            summary['tolerance'].append(max( summary['tolerance'], float(tol)))
                            if nexttol: nexttol = False
                        except:
                            nexttol = not nexttol

                if nfailed==0 and npassed>0:
                    summary['passed'] = True

                #import pdb; pdb.set_trace()
                print('NO. of verifications: %d'%(npassed+nfailed))
                print('NO. of passed verifications: ', npassed)
                print('NO. of failed verifications: ', nfailed)
                print('Tolerance for verification: ', '{:22.16f}'.format(summary['tolerance']) )
                print('')
                if len(summary['difference'])>0:
                    print('The smallest Normalized RMS difference: ', min(summary['difference']) )
                    print('The average Normalized RMS difference: ', sum(summary['difference'])/float(len(summary['difference'])) )
                    print('The largest Normalized RMS difference: ', max(summary['difference']) )
                    print('')
                if len(summary['elapsed_time'])>0:
					print('The minimum elapsed time (usec): ', '{:20.3f}'.format(min(summary['elapsed_time'])) )
					print('The average elapsed time (usec): ', '{:20.3f}'.format(sum(summary['elapsed_time'])/float(len(summary['elapsed_time']))) )
					print('The maximum elapsed time (usec): ', '{:20.3f}'.format(max(summary['elapsed_time'])) )
					print('')

                out, err = run_shcmd('perf stat -- make -f %s run'%MAKEFILE, stderr_exit=False, cwd=dirName)
                summary['perf_stat'] = err

            except Exception as e:
                print ('ERROR: %s'%str(e))
                sys.exit(-1)
            finally:
                out, err = run_shcmd('make -f %s clean'%MAKEFILE, stderr_exit=False, cwd=dirName)

    tests['end'] = str(datetime.datetime.now())
    ######### END OF TEST DATA COLLECTION ###########

    ######### SAVING TEST DATA IN A JSON FILE ###########
    with open('%s_result.json'%tests['git_branch'], 'w') as f:
        json.dump(tests, f, sort_keys=True, indent=4)

if __name__ == '__main__':
    main()
