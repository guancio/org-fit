import tcxparser
from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

plt.xkcd()

#filename = '/home/guancio/Sources/org-fit/data/activity_3016769645.tcx'
#problems with output from runalyze
#filename = '/home/guancio/Sources/org-fit/data/7982-Activity_2018-10-29_18-18_4714045.tcx'
# filename = '/home/guancio/Sources/org-fit/activity_3029988752.tcx'
tcx_filename = '/home/guancio/Sources/org-fit/data/activity_3129411144.tcx'
tcx_filename = '/home/guancio/Sources/org-fit/data/activity_3129411414.tcx'

last_tcx = None

value = "hr"

def reset_last():
    global last_tcx
    last_tcx = None

def parse_tcx(filename):
    global last_tcx
    if last_tcx is not None and last_tcx["filename"] == filename:
        return last_tcx["tr"]
    
    tcx = tcxparser.TCXParser(filename)
    train = {}
    train['activity_type'] = tcx.activity_type
    train['calories'] = tcx.calories
    train['distance_units'] = tcx.distance_units
    train['date'] = tcx.time_values()[0]
    tr = pd.DataFrame({ 'time': [], 'hr' : []})
    t1 = datetime.strptime(tcx.time_values()[0][:-1], "%Y-%m-%dT%H:%M:%S.%f")
    for i in range(min(len(tcx.time_values()), len(tcx.hr_values()))):
        t = (datetime.strptime(tcx.time_values()[i][:-1], "%Y-%m-%dT%H:%M:%S.%f") - t1).total_seconds()
        tr = tr.append({"time" : t,
                        "hr": tcx.hr_values()[i]},
                       ignore_index=True)
    tr["duration"] =  -tr["time"].diff(periods=-1)
    last_tcx = {"filename": filename, "tr": tr}

def draw_line_graph(tcx_filename, value, filename):
    to_plot = parse_tcx(tcx_filename)
    if to_plot is None:
        return
    to_plot = to_plot.set_index('time')
    fig, ax = plt.subplots(nrows=1, ncols=1)
    ax.plot(to_plot[value])
    plt.legend()
    fig.savefig(filename)
    plt.close(fig)
    return filename

def draw_histogram(tcx_filename, value, filename):
    to_plot = parse_tcx(tcx_filename)
    if to_plot is None:
        return
    to_plot = to_plot.dropna()
    bins_hr = np.linspace(100, 180, 5)  # number of bins in the histograms
    fig, ax = plt.subplots(nrows=1, ncols=1)
    ax.hist(to_plot[value], weights=to_plot["duration"], bins=bins_hr)
    fig.savefig(filename)
    plt.close(fig)
    return filename
