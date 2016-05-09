#!/usr/bin/env python
''' Script to plot test results collected by test_kernels.py

    Author: Youngsung Kim (youngsun@ucar.edu)

    Version: 0.1

    History:
    - Created at May 6, 2016
'''

import re
import sys
import json
import math

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
SUBTITLE_SIZE = 18
LABEL_SIZE = 18 
REF_NAME = 'HSW'
#REF_NAME = 'SNB'
#REF_NAME = 'KNC'
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

                                posp = rv.find('%')
                                if posp>=0:
                                    rval = float(rv[:posp])*0.01
                                else:
                                    rval = float(rv)

                                posp = ru.find('(')
                                if posp>=0:
                                    runit = ru[:posp].strip()
                                else:
                                    runit = ru.strip()
                        except: pass

                    try:
#                        if event in [ 'task-clock (msec)', 'context-switches', 'cpu-migrations', 'page-faults', \
#                            'cycles', 'stalled-cycles-frontend', 'stalled-cycles-backend', \
#                            'instructions', 'branches', 'branch-misses' ]:

                        if event in [ 'cycles', 'instructions', 'branches', 'branch-misses' ]:
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
        if len(case['etime'])>0:
            avg_etime = sum(case['etime']) / len(case['etime'])
            ref_etime[casename] = avg_etime

    plot_etime = {}
    #plot_diff = {}

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
                    #plot_perf[casename] = [ {} ] * len(platform_labels)
                    plot_perf[casename] = []
                    for _ in range(len(platform_labels)): 
                        plot_perf[casename].append({})
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

#            if not plot_diff.has_key(casename):
#                plot_diff[casename] = [ math.float('nan') ] * len(platform_labels)
#            if len(case['diff'])>0 and all(isinstance(f, float) and not math.isnan(f) for f in case['diff']):
#                avg_diff = sum(case['diff']) / len(case['diff'])
#                plot_diff[casename][platform_labels.index(platform)] =  avg_diff

    #import pdb; pdb.set_trace()

    # front page
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 1, 1, 0])
    ax.text(0.5, 0.3, 'Single-thread Performance Evaluation', fontsize=TITLE_SIZE, \
        horizontalalignment='center', verticalalignment='center')
    ax.text(0.5, 0.4, 'of the latest Intel Platforms', fontsize=TITLE_SIZE, \
        horizontalalignment='center', verticalalignment='center')
    ax.text(0.5, 0.7, 'May 10, 2016', fontsize=TITLE_SIZE, \
        horizontalalignment='center', verticalalignment='center')
    ax.text(0.5, 0.8, 'Youngsung Kim', fontsize=TITLE_SIZE, \
        horizontalalignment='center', verticalalignment='center')
    ax.text(0.5, 0.9, '( Application Scalability and Performance Group, NCAR )', fontsize=TITLE_SIZE, \
        horizontalalignment='center', verticalalignment='center')
    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)

    # platform page
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 10, -20, 0])
    ax.text(1, -0.5, 'Test Platforms and a compiler', fontsize=TITLE_SIZE)
    j = -3 
    for platform, test in tests.items():
        ax.text(1, j, platform)
        j -= 1
        ax.text(3, j, 'CPU Model Name : %s'%test['cpuname'])
        #j -= 1
        #ax.text(3, j, 'Test Date/Time : %s'%test['testdatetime'])
        j -= 1
        ax.text(3, j, 'Compiler : %s'%test['ifort'])
        j -= 1
        ax.text(3, j, '')
        j -= 1
        ax.text(3, j, '')
    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)

    plot_etime_list = plot_etime.items()
    plot_etime_list.sort( key = lambda (k,v): k.lower())

    # kernels page
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 10, -1*len(plot_etime), 0])
    ax.text(1, -1, 'Climate KGen Kernels', fontsize=TITLE_SIZE)
    ax.text(0, -3, '* Available from https://github.com/NCAR/kernelOptimization')
    ax.text(0, -4.5, '* Extracted using KGen(https://github.com/NCAR/KGen)')
    ax.text(0, -6, '   from CESM, HOMME, PORT(RRTMG), and MPAS')
    half = int(len(plot_etime)/2.0)
    row1 = 5
    row2 = 5
    #for i, (casename, etime) in enumerate(plot_etime.items()):
    for i, (casename, etime) in enumerate(plot_etime_list):
        if i < half:
            if len(casename)>41:
                ax.text(0, (-row1 - 1)*1.5, '(%d) %s'%(i, casename[:41]), ha='left')
                row1 += 1
                ax.text(0.2, (-row1 - 1)*1.5, '%s'% casename[41:], ha='left')
            else:
                ax.text(0, (-row1 - 1)*1.5, '(%d) %s'%(i, casename), ha='left')
            row1 += 1
    #for i, (casename, etime) in enumerate(plot_etime.items()):
    for i, (casename, etime) in enumerate(plot_etime_list):
        if i >= half:
            if len(casename)>41:
                ax.text(5, (-row2-1)*1.5, '(%d) %s'%(i, casename[:41]), ha='left')
                row2 += 1
                ax.text(5.2, (-row2-1)*1.5, '%s'%casename[41:], ha='left')
            else:
                ax.text(5, (-row2-1)*1.5, '(%d) %s'%(i, casename), ha='left')
            row2 += 1

    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)


    # test introduction page
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 1, -10, 0])
    ax.text(0, -1, 'How to rerun the tests:', fontsize=TITLE_SIZE)
    ax.text(0, -2, '   >> git clone https://github.com/NCAR/kernelOptimization.git', fontsize=SUBTITLE_SIZE)
    ax.text(0, -3, '   >> cd kernelOptimization', fontsize=SUBTITLE_SIZE)
    ax.text(0, -4, '   >> git checkout [snb_tag_org|hsw_tag_org|knc_tag_org|knl_tag_org]', fontsize=SUBTITLE_SIZE)
    ax.text(0, -5, '   >> ./scripts/test_kernels.py .', fontsize=SUBTITLE_SIZE)
    ax.text(0, -6, '   NOTE: It is assumed that you are on one of test platforms.', fontsize=SUBTITLE_SIZE)
    ax.text(0, -7, '         You may need to modify Makefile(s) to fit to your test env.', fontsize=SUBTITLE_SIZE)
    ax.text(0, -8, 'Raw test results for this report are available:', fontsize=TITLE_SIZE)
    ax.text(0, -9, '   >> cd testdata/May_05_2016', fontsize=SUBTITLE_SIZE)

    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)

    # page break
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 1, 1, 0])
    ax.text(0.5, 0.5, 'Performance Comparison to HSW (Single Thread)', fontsize=TITLE_SIZE, \
        horizontalalignment='center', verticalalignment='center')
    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)


    # average performance ratio page
    plot_sumetime = [ 0.0 ] * len(platform_labels)
    for cidx, (casename, etime) in enumerate(plot_etime.items()):
        for i in range(len(platform_labels)):
            #plot_sumetime[i] += plot_etime[casename][i]
            plot_sumetime[i] += etime[i]
    fig = plt.figure(figsize=(10, 6))
    width = 0.7
    interval = 1.0
    xticks = []
    for i, platform in enumerate(platform_labels):
        y = plot_sumetime[i]/plot_sumetime[platform_labels.index(REF_NAME)]
        barplot = plt.bar(i + interval/2 - width/2, y, width, color=color_names[i])
        plt.text(i + interval/2, y+0.05, "{:5.2f}".format(y), ha='center', va='bottom')
        xticks.append(i + interval/2)
    plt.title('All KGen kernels(%d) - combined'%len(ref_cases), fontsize=TITLE_SIZE)
    plt.ylim([ 0.0, 1.1 ])
    plt.xlabel('platform (single thread)', fontsize=LABEL_SIZE)
    plt.ylabel('Relative performance to %s'%REF_NAME, fontsize=LABEL_SIZE)
    plt.xticks(xticks, platform_labels)
    pdf.savefig(fig)

    # histogram of average performance ratio page

    # veirification summary
#    plot_sumdiff= [] 
#    for _ in range(len(platform_labels)):
#        plot_sumdiff.append([])
#    for i in range(len(platform_labels)):
#        casediff = []
#        for casename, platform_diff in plot_diff.items():
#            if isinstance(platform_diff[i], float) and not math.isnan(platform_diff[i]):
#                plot_sumdiff[i].append(platform_diff[i])
#
#    fig = plt.figure(figsize=(10, 6))
#    width = 0.7
#    interval = 1.0
#    xticks = []
#    for i, platform in enumerate(platform_labels):
#        y = sum(plot_sumdiff[i]) / len(plot_sumdiff[i])
#        barplot = plt.bar(i + interval/2 - width/2, y, width, color=color_names[i])
#        plt.text(i + interval/2, y+0.05, "{:5.2f}".format(y), ha='center', va='bottom')
#        xticks.append(i + interval/2)
#    plt.title('All KGen kernels(%d)'%len(ref_cases), fontsize=TITLE_SIZE)
#    #plt.ylim([ 0.0, 1.1 ])
#    plt.xlabel('platform', fontsize=LABEL_SIZE)
#    plt.ylabel('Normalized RMS Difference to SandyBridge', fontsize=LABEL_SIZE)
#    plt.xticks(xticks, platform_labels)
#    pdf.savefig(fig)

    # relative pages
    width = 0.7
    interval = 1.0
    #for cidx, (casename, etime) in enumerate(plot_etime.items()):
    for cidx, (casename, etime) in enumerate(plot_etime_list):
        fig = plt.figure(figsize=(10, 6))
        xticks = []
        for i in range(len(platform_labels)):
            barplot = plt.bar(i + interval/2 - width/2, etime[i], width, color=color_names[i])
            if etime[i]>0:
                plt.text(i + interval/2, etime[i]+0.05, "{:5.2f}".format(etime[i]), ha='center', va='bottom')
            xticks.append(i + interval/2)
        #plt.legend(barplots, platform_labels)
        plt.title('(%d) %s'%(cidx,casename), fontsize=TITLE_SIZE)
        plt.ylim([ 0.0, max(etime)*1.1 ])
        plt.xlabel('platform (single thread)', fontsize=LABEL_SIZE)
        plt.ylabel('Relative performance to %s'%REF_NAME, fontsize=LABEL_SIZE)
        plt.xticks(xticks, platform_labels)
        pdf.savefig(fig)
        #plt.show()

    # page break
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis([0, 1, 1, 0])
    ax.text(0.5, 0.5, 'Cluster Analysis with Linux "perf stat" H/W counters', fontsize=TITLE_SIZE, \
        horizontalalignment='center', verticalalignment='center')
    ax.axis('off')
    fig.tight_layout()
    pdf.savefig(fig)


    # cluster analsys pages

    #for event in plot_perf_minmax['KNL']:
    for event in [ 'instructions', 'branch-misses', 'branches', 'cycles' ]:
        if event not in plot_perf_minmax['KNL']: continue

        fig, ax = plt.subplots(figsize=(10, 6))
        ax.set_title('Cluster Analysis of KGen kernels\n( relative performance vs. relative "%s" count )'%event)
        
        lim = [ float('inf'), float('-inf'), 0, 0.9 ]
        #ax.axis([minmax[0], minmax[1], 0, 0.9])
        #for i, (casename, etime) in enumerate(plot_etime.items()):
        for i, (casename, etime) in enumerate(plot_etime_list):
            if plot_perf[casename][platform_labels.index('KNL')][event][0] and plot_perf[casename][platform_labels.index('HSW')][event][0]:
                #ax.text(plot_perf[casename][platform_labels.index('KNL')][event][0], etime[platform_labels.index('KNL')], '(%d)'%i, ha='center')
                x = float(plot_perf[casename][platform_labels.index('KNL')][event][0]) / plot_perf[casename][platform_labels.index('HSW')][event][0]
                lim[0] = min(lim[0], x)
                lim[1] = max(lim[1], x)
                ax.text(x, etime[platform_labels.index('KNL')], '(%d)'%i, ha='center')
        #ax.axis([0,5, 0, 0.9])
        ax.set_xlabel('KNL relative "%s" to %s'%(event, REF_NAME), fontsize=LABEL_SIZE)
        ax.set_ylabel('KNL relative performance to %s'%REF_NAME, fontsize=LABEL_SIZE)
        ax.axis(lim)
        pdf.savefig(fig)

        fig, ax = plt.subplots(figsize=(10, 6))
        if plot_perf[casename][platform_labels.index('KNL')][event][2]:
            unit = plot_perf[casename][platform_labels.index('KNL')][event][2]
        else:
            unit = ''
        ax.set_title('Cluster Analysis of KGen kernels\n( relative performance vs. relative "%s" ratio %s ) '%\
            (event, unit))
        #ax.axis([minmax[2], minmax[3], 0, 0.9])
        lim = [ float('inf'), float('-inf'), 0, 0.9 ]
        #ax.axis([0, 5, 0, 0.9])
        for i, (casename, etime) in enumerate(plot_etime.items()):
            if plot_perf[casename][platform_labels.index('KNL')][event][1] and plot_perf[casename][platform_labels.index('HSW')][event][1]:
                #ax.text(plot_perf[casename][platform_labels.index('KNL')][event][1], etime[platform_labels.index('KNL')], '(%d)'%i, ha='center')
                x = plot_perf[casename][platform_labels.index('KNL')][event][1] / plot_perf[casename][platform_labels.index('HSW')][event][1]
                lim[0] = min(lim[0], x)
                lim[1] = max(lim[1], x)
                ax.text(x, etime[platform_labels.index('KNL')], '(%d)'%i, ha='center')
        ax.set_xlabel('KNL relative "%s" ratio to %s'%(event, REF_NAME), fontsize=LABEL_SIZE)
        ax.set_ylabel('KNL relative performance to %s'%REF_NAME, fontsize=LABEL_SIZE)
        ax.axis(lim)
        pdf.savefig(fig)

#    # diff pages
#    width = 0.7
#    interval = 1.0
#    for cidx, (casename, diff) in enumerate(plot_diff.items()):
#        fig = plt.figure(figsize=(10, 6))
#        xticks = []
#        for i in range(len(platform_labels)):
#            if isinstance(diff[i], float) and not math.isnan(diff[i]):
#                barplot = plt.bar(i + interval/2 - width/2, diff[i], width, color=color_names[i])
#                plt.text(i + interval/2, diff[i]+0.05, "{0:.5e}".format(diff[i]), ha='center', va='bottom')
#                xticks.append(i + interval/2)
#        #plt.legend(barplots, platform_labels)
#        plt.title('DIFF (%d) %s'%(cidx,casename), fontsize=TITLE_SIZE)
#        #plt.ylim([ 0.0, max([ f for f in diff if isinstance(f, float) and not math.isnan(f)])*1.1 ])
#        plt.ylim([ 0.0, max(diff)*1.1 ])
#        plt.xlabel('platform', fontsize=LABEL_SIZE)
#        plt.ylabel('Normalized RMS Diff', fontsize=LABEL_SIZE)
#        plt.xticks(xticks, platform_labels)
#        pdf.savefig(fig)
#        #plt.show()


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
