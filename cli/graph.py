#!/usr/bin/python

from datetime import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


def prepare_data(trains, value, groupby, months, muscle):
    to_process = trains.copy()
    to_process = to_process.set_index('date')
    to_process= to_process.sort_index()
    if months is not None:
        to_process = to_process.last('%dM'%months)
    if muscle is not None:
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

# import orgparser
# trains.groupby('muscle').count()
# trains.groupby('muscle').count().plot.pie(subplots=True)
# trains.groupby('muscle').count()['count'].plot.pie()
# trains = orgparser.parse_gym_file("/home/guancio/Sources/org-fit/data/res.org")

def draw_line_graph(trains, value, groupby, months, muscle, filename):
    to_plot = prepare_data(trains, value, groupby, months, muscle)
    if to_plot is None:
        return
    fig, ax = plt.subplots(nrows=1, ncols=1)
    ax.plot(to_plot)
    plt.title(value)
    plt.legend()
    fig.savefig(filename)
    #plt.show()
    plt.close(fig)

if (0):
    to_process = trains.copy()
    to_process = to_process.set_index('date')
    to_process= to_process.sort_index()
    filtered = to_process
    filtered = to_process.last('1M')
    filtered = to_process.last('1W')

    filtered['volume'] = filtered['weight'] * filtered['count']
    filtered.set_index('muscle')
    filtered['reps'] = filtered['count']
    grouped = filtered.groupby('muscle')
    values = grouped.agg({
        'count': np.size,
        'volume': np.sum,
        'reps' : np.sum
    })

    fig, ax = plt.subplots(nrows=1, ncols=1)
    values.plot.pie(subplots=True)
    plt.show()
    plt.close(fig)

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
