#!/usr/bin/python

from datetime import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
plt.xkcd()

next_file_id = 1

def prepare_data(trains, value, groupby, months, muscle):
    to_process = trains.copy()
    to_process = to_process.set_index('date')
    to_process= to_process.sort_index()
    if months != "all":
        to_process = to_process.last('%dM'%int(months))
    if muscle != "all":
        to_process = to_process[to_process['muscle'] == muscle]
    group_freq = None
    if groupby == "day":
        group_freq = 'D'
    elif groupby == "week":
        group_freq = 'W-MON'
    elif groupby == "month":
        group_freq = 'MS'
    else:
        return None
    to_process['volume'] = to_process['weight'] * to_process['count']
    to_process['reps'] = to_process['count']
    if (value != "count"):
        grouped = to_process.groupby(pd.Grouper(freq=group_freq))
        if value == "sets":
            values = grouped.agg({'count': np.size})
        elif value == "reps":
            values = grouped.agg({'count': np.sum})
        elif value == "vol":
            values = grouped.agg({'volume': np.sum})
        else:
            return None
    else:
        grouped = to_process.groupby(pd.Grouper(freq='D'))
        values = grouped.agg({'count': np.size})
        values = values[values['count'] > 0]
        grouped2 = values.groupby(pd.Grouper(freq=group_freq))
        values = grouped2.agg({'count': np.size})

    k = values.keys()[0]
    values = values[values[k] > 0]
    return values

def get_all_muscles(trains):
    return trains.groupby('muscle').count()


def get_file_name(filename):
    global next_file_id
    if not filename.endswith(".png"):
        filename += "%d.png" % next_file_id
        next_file_id += 1
    return filename

def draw_line_graph(trains, value, groupby, months, muscle, filename):
    to_plot = prepare_data(trains, value, groupby, months, muscle)
    if to_plot is None:
        return
    fig, ax = plt.subplots(nrows=1, ncols=1)
    ax.plot(to_plot)
    plt.title(value)
    plt.legend()
    filename = get_file_name(filename)
    fig.savefig(filename)
    #plt.show()
    plt.close(fig)
    return filename


def draw_pie_graph(trains, value, period, filename):
    to_process = trains.copy()
    to_process = to_process.set_index('date')
    to_process= to_process.sort_index()
    filtered = to_process
    if period == "week":
        filtered = to_process.last('1M')
    elif period == "month":
        filtered = to_process.last('1W')
    elif period == "all":
        filtered = to_process
    else:
        return None

    filtered['volume'] = filtered['weight'] * filtered['count']
    filtered.set_index('muscle')
    filtered['reps'] = filtered['count']
     
    if (value != "count"):
        grouped = filtered.groupby('muscle')
        if value == "sets":
            values = grouped.agg({'count': np.size})
        elif value == "reps":
            values = grouped.agg({'reps': np.sum})
        elif value == "vol":
            values = grouped.agg({'volume': np.sum})
        else:
            values = None
    else:
        return None
        grouped = to_process.groupby(pd.Grouper(freq='D'))
        values = grouped.agg({'count': np.size})
        values = values[values['count'] > 0]
        grouped2 = values.groupby('muscle')
        values = grouped2.agg({'count': np.size})
     
    fig, ax = plt.subplots(nrows=1, ncols=1)
    plt.axis('equal')
    ax.pie(values, autopct='%1.0f%%', labels=values.index.values)
    plt.title(value)
    filename = get_file_name(filename)
    fig.savefig(filename)
    plt.close(fig)
    return filename

    
if (0):
    to_process = trains.copy()
    to_process = to_process.set_index('date')
    to_process= to_process.sort_index()
    to_process['volume'] = to_process['weight'] * to_process['count']
    to_process = to_process[to_process['muscle'] == 'Abs']
    grouped = to_process.groupby(pd.Grouper(freq='D'))
    values = grouped.agg({
        # 'count': np.size,
        # 'volume': np.sum,
        # 'reps' : np.sum,
        'weight' : np.max
    })
    k = values.keys()[0]
    values = values[values[k] > 0]
    values.plot()
    plt.show()
