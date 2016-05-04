#!/bin/env python
'''Python script for running kgen kernels in this repo.'''

from __future__ import print_function
import os
import sys
import json
import datetime
import subprocess

SCRIPT_HOME, SCRIPT_NAME = os.path.split(os.path.realpath(__file__))
KERNEL_HOME = '%s/..'%SCRIPT_HOME
MAKEFILE = 'Makefile'
SIGNATURE = '# Makefile for KGEN-generated kernel\n'
FC = 'FC'
FC_FLAGS = 'FC_FLAGS'
TEMP = '__temp__'

tests = {}

def run_shcmd(cmd, input=None, **kwargs):
    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
    return proc.communicate(input=input)

def main():
    pass

    if len(sys.argv) > 1:
        testroots = sys.argv[1:]
    else:
        print ('Please specify the paths to be tested or "all" for all tests.')
        print ('Usage %s [ testroot [ testroot ... ] | all ]'%sys.argv[0])
        sys.exit(-1)
        
    # check if it is a kgen kernel directory that can be executed
    # run the kernel
    # walk through sub directories
    tests = {}

    out, err = run_shcmd('env')
    tests['env'] = out.rstrip()

    out, err = run_shcmd('ifort --version')
    tests['ifort'] = out.rstrip()

    out, err = run_shcmd('git rev-parse --abbrev-ref HEAD')
    tests['gitbranch'] = out.rstrip()

    out, err = run_shcmd('git rev-parse HEAD')
    tests['gitcommit'] = out.rstrip()

    out, err = run_shcmd('git diff')
    tests['gitdiff'] = out.rstrip()

    out, err = run_shcmd('cat /proc/cpuinfo')
    tests['cpuinfo'] = out.rstrip()

    out, err = run_shcmd('cat /proc/meminfo')
    tests['meminfo'] = out.rstrip()

    out, err = run_shcmd('top -n1 -b')
    tests['top'] = out.rstrip()

    tests['begin'] = str(datetime.datetime.now())

    tests['tests'] = {}

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

            #print('Running a kernel at %s: '%relpath, end='')
            # modify makefile list

            try:
                summary = {'etime': [], 'diff': [], 'tol': 0.0, 'passed': False, 'begin': str(datetime.datetime.now()), 'end': 'Not completed'}
                tests['tests'][dirName] = summary

                # run test
                out, err = run_shcmd('make -f %s'%MAKEFILE, cwd=dirName)

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
                            if outline.find('usec')>=0:
                                summary['etime'].append(float(etime))
                            elif outline.find('sec')>=0:
                                summary['etime'].append(float(etime)*1000000.0)
                            if nexttime: nexttime = False
                        except:
                            nexttime = not nexttime
                            
                    if nextdiff or outline.find('Normalized RMS of difference')>=0:
                        try:
                            diff = float(outline.split()[-1])
                            summary['diff'].append(float(diff))
                            if nextdiff: nextdiff = False
                        except:
                            nextdiff = not nextdiff

                    if nexttol or outline.find('tolerance')>=0 or outline.find('Tolerance')>=0:
                        try:
                            tol = float(outline.split()[-1])
                            summary['tol'].append(max( summary['tol'], float(tol)))
                            if nexttol: nexttol = False
                        except:
                            nexttol = not nexttol

                if nfailed==0 and npassed>0:
                    summary['passed'] = True

                #import pdb; pdb.set_trace()
                print('NO. of verifications: %d'%(npassed+nfailed))
                print('NO. of passed verifications: ', npassed)
                print('NO. of failed verifications: ', nfailed)
                print('Tolerance for verification: ', '{:22.16f}'.format(summary['tol']) )
                print('')
                if len(summary['diff'])>0:
                    print('The smallest Normalized RMS difference: ', min(summary['diff']) )
                    print('The average Normalized RMS difference: ', sum(summary['diff'])/float(len(summary['diff'])) )
                    print('The largest Normalized RMS difference: ', max(summary['diff']) )
                    print('')
                print('The minimum elapsed time (usec): ', '{:20.3f}'.format(min(summary['etime'])) )
                print('The average elapsed time (usec): ', '{:20.3f}'.format(sum(summary['etime'])/float(len(summary['etime']))) )
                print('The maximum elapsed time (usec): ', '{:20.3f}'.format(max(summary['etime'])) )
                print('')

                out, err = run_shcmd('perf stat -- make -f %s run'%MAKEFILE, cwd=dirName)
                summary['perf-stat'] = err.rstrip()

            except Exception as e:
                print ('ERROR: %s'%str(e))
                print ( err )
                #import pdb; pdb.set_trace()    
            finally:
                out, err = run_shcmd('make -f %s clean'%MAKEFILE, cwd=dirName)

    tests['end'] = str(datetime.datetime.now())

    with open('%s_result.json'%tests['gitbranch'], 'w') as f:
        json.dump(tests, f, sort_keys=True, indent=4)

if __name__ == '__main__':
    main()
