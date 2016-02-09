'''Python script for running kgen kernels in this repo.'''

from __future__ import print_function
import os
import sys
import subprocess
import shutil

SCRIPT_HOME, SCRIPT_NAME = os.path.split(os.path.realpath(__file__))
KERNEL_HOME = '%s/..'%SCRIPT_HOME
MAKEFILE = 'Makefile'
SIGNATURE = '# Makefile for KGEN-generated kernel\n'
FC = 'FC :='
FC_FLAGS = 'FC_FLAGS :='
ORG = '__org__'

tests = {}

def run_shcmd(cmd, input=None, **kwargs):
    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
    return proc.communicate(input=input)

def main():
    pass

    # check if it is a kgen kernel directory that can be executed
    # run the kernel
    # walk through sub directories
    N = 0
    for dirName, subdirList, fileList in os.walk(KERNEL_HOME):
        relpath = os.path.relpath(dirName, KERNEL_HOME)
        if relpath==SCRIPT_HOME.split('/')[-1]: del subdirList[:]
        if MAKEFILE not in fileList: continue

        makefile = []
        with open(os.path.join(dirName, MAKEFILE), 'r') as f:
            makefile = f.readlines()
            if len(makefile)<4: continue
            if makefile[0]!=SIGNATURE: continue
            if not makefile[2].startswith(FC): continue
            if not makefile[3].startswith(FC_FLAGS): continue

        print('Running a kernel at %s: '%relpath, end='')
        test = {}
        tests[relpath] = test

        # modify makefile list

        # save original Makefile
        mksrc = os.path.join('%s/%s'%(dirName, MAKEFILE))
        mkdst = os.path.join('%s/%s.%s'%(dirName, MAKEFILE, ORG))
        if not os.path.exists(mkdst):
            shutil.copyfile(mksrc, mkdst)

        # replace Makefile with makefile ist
        with open(os.path.join(dirName, MAKEFILE), 'w') as f:
            f.writelines(makefile)
 
        # run test
        try:
            out, err = run_shcmd('make', cwd=dirName)

            # save test id, and outputs
            test['out'] = out.split('\n')
            test['err'] = err.split('\n')

            # collect summary of the test results
            summary = {'etime': []}
            test['summary'] = summary
            npassed = 0
            nfailed = 0
            for outline in test['out']:
                if outline.find('FAILED')>=0:
                    nfailed += 1
                elif outline.find('PASSED')>=0:
                    npassed += 1

                if outline.find('time')>=0 or outline.find('Time')>=0:
                    try:
                        etime = float(outline.split()[-1])
                        if outline.find('usec')>=0:
                            summary['etime'].append(float(etime)/1000000.0)
                        elif outline.find('sec')>=0:
                            summary['etime'].append(float(etime))
                    except: pass
                        
            if nfailed>0:
                summary['passed'] = False
                print('FAILED')
            if npassed==0:
                summary['passed'] = False
                print('FAILED')
            else:
                summary['passed'] = True
                if npassed != len(summary['etime']): print('XX: ', npassed. len(summary['etime']))
                print('PASSED (%f seconds)'%(sum(summary['etime'])/float(npassed)))
        except Exception as e:
            import pdb; pdb.set_trace()    

        # recover Makefile
        shutil.move(mkdst, mksrc)

        print('')
        #if N>1: break
        #N += 1

    print('***************************************************************')
    print('                          TEST RESULT                          ')
    print('***************************************************************')

    ntests = 0
    npassed = 0
    for relpath, test in tests.items():
        summary = test['summary']
        ntests += 1
        if summary['passed']: npassed += 1

    print('NO. of tests: %d'%ntests)
    print('NO. of passed tests: %d'%npassed)
    print('NO. of failed tests: %d'%(ntests - npassed))

if __name__ == '__main__':
    main()
