#!/bin/env python
''' Script to plot test results collected by test_kernels.py

    Author: Youngsung Kim (youngsun@ucar.edu)

    Version: 0.1

    History:
    - Created at May 6, 2016
'''

import re
import sys
import json

try:
    import matplotlib
    import matplotlib.pyplot as plt
    from matplotlib.backends.backend_pdf import PdfPages

    color_names = []
    for name, hex in matplotlib.colors.cnames.iteritems():
        color_names.append(name)

    pdf = PdfPages('kgen_kernels.pdf')
except:
    print 'ERROR: matplotlib module is not loaded.'
    sys.exit(-1)

TITLE_SIZE = 24
LABEL_SIZE = 18 
#REF_NAME = 'HSW'
#REF_NAME = 'SNB'
REF_NAME = 'KNC'
#REF_NAME = 'KNL'
CPU_MODEL_NAME = re.compile(u'^model\sname[\t\s]+:\s(?P<modelname>.+$)', re.M)

def getinfo(string, pattern, method='search', **flags):
    out = None
    try:
        exec('out = pattern.%s(string, **flags)'%method)
    except:
        pass
    return out

def main():

    in_tests = []

    for in_test in sys.argv[1:]:
        try:
            with open(in_test, 'r') as f:
                in_tests.append(json.load(f))        
        except Exception as e:
            print str(e)
            sys.exit(-1)

    platform_labels = []

    tests = {}
    # tests
    #   cpuname
    #   testdatetime
    #   ifort
    #   cases: dict
    #       <casepath>
    #           etime: list of floats
    #           diff: list of floats
    #           tol: float
    #           perf: dict
    #               <perf-events>: [ counter, ratio ]

    # collect data from raw data
    for in_test in in_tests:
        try:
            # collect common in_test attributes
            cpuname = getinfo(in_test[u'cpu_model'], CPU_MODEL_NAME).group('modelname')
            in_testbegin = in_test[u'begin']
            in_testend = in_test[u'end']
            #env = in_test[u'env']
            #top = in_test[u'top']
            gitcommit = in_test[u'git_commit']
            meminfo = in_test[u'total_memsize']
            gitbranch = in_test[u'git_branch']
            ifort = in_test[u'compiler'].split('\n')[0]

            platform_label = gitbranch[:3].upper()
            platform_labels.append(platform_label)

            test = {}
            tests[platform_label] = test
            
            test['cpuname'] = cpuname
            test['testdatetime'] = in_testend
            test['ifort'] = ifort

            cases = {}
            test['cases'] = cases

            for in_testpath, in_testcase in in_test[u'cases'].items():
                # collect in_test case attributes

                etime = in_testcase[u'elapsed_time']
                if len(etime)==0: continue
                #if sum(etime)/len(etime)<10.0: continue
                if any([ e<=0 for e in etime ]): continue

                passed = in_testcase[u'passed']
                diff = in_testcase[u'difference']
                diff_type = in_testcase[u'diff_type']
                casebegin = in_testcase[u'begin']
                caseend = in_testcase[u'end']
                tol = in_testcase[u'tolerance']

                perf = {}
                for line in in_testcase[u'perf_stat'].split('\n'):
                    if len(line)<49: continue
                    valuestr = line[:20].strip()
                    event = line[20:48].strip()
                    ratiostr = line[48:].strip()
                    rval, runit = None, None
                    if ratiostr:
                        try:
                            possharp = ratiostr.find('#')
                            if possharp>=0:
                                rstr = ratiostr[possharp+1:].strip()
                                rv, ru = rstr.split(None, 1)
                                rval = float(rv)
                                runit = ru.strip()
                        except: pass

                    try:
                        if event in [ 'task-clock (msec)', 'context-switches', 'cpu-migrations', 'page-faults', \
                            'cycles', 'stalled-cycles-frontend', 'stalled-cycles-backend', \
                            'instructions', 'branches', 'branch-misses' ]:
                            value = float(valuestr.replace(',',''))
                            perf[event] = [ value, rval, runit ]
                    except: pass

                case = {}
                cases[in_testpath] = case

                case['etime'] = etime
                case['diff'] = diff
                case['tol'] = tol
                case['perf'] = perf
        except Exception as e:
            print str(e)
            sys.exit(-1)
                

    # let reference platform on the first column
    platform_labels[platform_labels.index(REF_NAME)], platform_labels[0] = \
        platform_labels[0], platform_labels[platform_labels.index(REF_NAME)]

    # collect reference data from reference platform
    ref_data = tests[REF_NAME]
    ref_cases = ref_data['cases']

    ref_etime = {}
    for casename, case in ref_cases.items():
        avg_etime = sum(case['etime']) / len(case['etime'])
        ref_etime[casename] = avg_etime

    plot_etime = {}
    # plot_etime: dict
    #   <casename>: [ platform_1, ... platform_n ]

    plot_perf = {}
    plot_perf_minmax = {}

    for platform, test in tests.items():

        for casename, case in test['cases'].items():
            if casename in ref_etime:
                # etime
                if not plot_etime.has_key(casename):
                    plot_etime[casename] = [ 0.0 ] * len(platform_labels)
                avg_etime = sum(case['etime']) / len(case['etime'])
                plot_etime[casename][platform_labels.index(platform)] =  ref_etime[casename] / avg_etime

                # perf events
                if not plot_perf.has_key(casename):
                    plot_perf[casename] = [ {} ] * len(platform_labels)
                for event, evals in case['perf'].items():
                    plot_perf[casename][platform_labels.index(platform)][event] = evals
                    if not plot_perf_minmax.has_key(platform):
                        plot_perf_minmax[platform] = {}
                    if not plot_perf_minmax[platform].has_key(event):
                        plot_perf_minmax[platform][event] = [ float("inf"), float("-inf"),float("inf"),float("-inf") ]
                    plot_perf_minmax[platform][event][0] = min(plot_perf_minmax[platform][event][0], evals[0])
                    plot_perf_minmax[platform][event][1] = max(plot_perf_minmax[platform][event][1], evals[0])
                    plot_perf_minmax[platform][event][2] = min(plot_perf_minmax[platform][event][2], evals[1])
                    plot_perf_minmax[platform][event][3] = max(plot_perf_minmax[platform][event][3], evals[1])

    #import pdb; pdb.set_trace()

    # general page
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 10, -20, 0])
    j = 0
    for platform, test in tests.items():
        ax.text(1, j, platform)
        j -= 1
        ax.text(3, j, 'CPU Model Name : %s'%test['cpuname'])
        j -= 1
        ax.text(3, j, 'Test Date/Time : %s'%test['testdatetime'])
        j -= 1
        ax.text(3, j, 'Compiler : %s'%test['ifort'])
        j -= 1
        ax.text(3, j, '')
        j -= 1
        ax.text(3, j, '')
    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)

    # kernels page
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 10, -1*len(plot_etime), 0])
    for i, (casename, etime) in enumerate(plot_etime.items()):
        ax.text(2, -i, '(%d) %s'%(i, casename), ha='left')
    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)

    # average performance ratio page


    # histogram of average performance ratio page

    # selective average performance ratio pages


    # cluster analsys pages

    for event, minmax in plot_perf_minmax[REF_NAME].items():
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.set_title('Cluster Analysis of KGen kernels by KNL-to-%s Performance and Performance Event %s counter'%(REF_NAME, event))
        ax.axis([minmax[0], minmax[1], 0, 0.7])
        for i, (casename, etime) in enumerate(plot_etime.items()):
            ax.text(plot_perf['KNL'][event][0], etime[platform_labels.index('KNL')], '(%d)'%i, ha='center')
        pdf.savefig(fig)

        fig, ax = plt.subplots(figsize=(10, 6))
        ax.set_title('Cluster Analysis of KGen kernels by KNL-to-%s Performance and Performance Event %s ratio (%s)'%(REF_NAME, event, plot_perf['KNL'][event][2]))
        ax.axis([minmax[2], minmax[3], 0, 0.7])
        for i, (casename, etime) in enumerate(plot_etime.items()):
            ax.text(plot_perf['KNL'][event][1], etime[platform_labels.index('KNL')], '(%d)'%i, ha='center')
        pdf.savefig(fig)

#    if event in [ 'task-clock (msec)', 'context-switches', 'cpu-migrations', 'page-faults', \
#        'cycles', 'stalled-cycles-frontend', 'stalled-cycles-backend', \
#        'instructions', 'branches', 'branch-misses' ]:
#        value = float(valuestr.replace(',',''))
#        perf[event] = [ value, rval, runit ]



    # cluster page
    fig, ax = plt.subplots(figsize=(10, 6))
    #ax.subtitle('kernels arranged by KNL/HSW performance ratio', fontsize=TITLE_SIZE)
    #ax.xlabel('Performance Ratio (KNL/HSW)', fontsize=LABEL_SIZE)
    ax.axis([0, 2, 0, 0.7])
    for i, (casename, etime) in enumerate(plot_etime.items()):
        ax.text(1, etime[platform_labels.index('KNL')], '(%d)'%i, ha='center')
    #ax.axis('off')
    #fig.tight_layout()
    pdf.savefig(fig)

    # relative pages
    width = 0.7
    interval = 1.0
    for casename, etime in plot_etime.items():
        fig = plt.figure(figsize=(10, 6))
        xticks = []
        for i in range(len(platform_labels)):
            barplot = plt.bar(i + interval/2 - width/2, etime[i], width, color=color_names[i])
            if etime[i]>0:
                plt.text(i + interval/2, etime[i]+0.05, "{:5.2f}".format(etime[i]), ha='center', va='bottom')
            xticks.append(i + interval/2)
        #plt.legend(barplots, platform_labels)
        plt.title(casename, fontsize=TITLE_SIZE)
        plt.ylim([ 0.0, max(etime)*1.1 ])
        plt.xlabel('platform', fontsize=LABEL_SIZE)
        plt.ylabel('Relative Performance to %s'%REF_NAME, fontsize=LABEL_SIZE)
        plt.xticks(xticks, platform_labels)
        pdf.savefig(fig)
        #plt.show()

    pdf.close()

        # create plots
                        # relative elapsed time plots per each test case


                        # clustering plots to relative elapsed time( KNL / HSW )
                        # many categories

        # summary plots
        # - combined relative epalsed time plot
        # - Normalized RMS difference plot
        # - combined clusterings plot
        # - 
        # - 

if __name__ == '__main__':
    main()
