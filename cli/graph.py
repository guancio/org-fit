#!/usr/bin/python

from datetime import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
plt.xkcd()

next_file_id = 1

def prepare_data(trains, groupby, months, muscle, exercise):
    to_process = trains.copy()
    to_process = to_process.set_index('date')
    to_process= to_process.sort_index()
    if months != "all":
        to_process = to_process.last('%dM'%int(months))
    if muscle != "all":
        to_process = to_process[to_process['muscle'] == muscle]
    if exercise != "all":
        to_process = to_process[to_process['exercise'] == exercise]
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
    to_process['sets'] = to_process['count']
    to_process['reps'] = to_process['count']
    to_process['max-weight'] = to_process['weight']
    to_process['max-reps'] = to_process['reps']
    to_process['epley'] = to_process['weight'] * (1 + to_process['reps']/30)
    grouped = to_process.groupby(pd.Grouper(freq=group_freq))
    values = grouped.agg({
        'sets': np.size,
        'reps' : np.sum,
        'volume' : np.sum,
        'max-weight' : np.max,
        'max-reps' : np.max,
        # rep max: TODO
        'epley' : np.max
    })

    k = values.keys()[0]
    values = values[values[k] > 0]
    return values

def get_all_muscles(trains):
    return trains.groupby('muscle').count()

def get_all_exercises(trains):
    return trains.groupby('exercise').count()


def get_file_name(filename):
    global next_file_id
    if not filename.endswith(".png"):
        filename += "%d.png" % next_file_id
        next_file_id += 1
    return filename

def draw_line_graph(trains, value, groupby, months, muscle, exercise, filename):
    to_plot = prepare_data(trains, groupby, months, muscle, exercise)
    if to_plot is None:
        return
    fig, ax = plt.subplots(nrows=1, ncols=1)
    ax.plot(to_plot[value])
    #plt.title(value)
    plt.legend()
    filename = get_file_name(filename)
    fig.savefig(filename)
    plt.close(fig)
    return filename


def prepare_breakout_data(trains, period):
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
    filtered['sets'] = filtered['count']
    grouped = filtered.groupby('muscle')
    values = grouped.agg({
        'sets': np.size,
        'reps' : np.sum,
        'volume' : np.sum
    })
    return values
    
def draw_pie_graph(trains, value, period, filename):
    values = prepare_breakout_data(trains, period)
     
    fig, ax = plt.subplots(nrows=1, ncols=1)
    plt.axis('equal')
    ax.pie(values[value], autopct='%1.0f%%', labels=values.index.values)
    plt.title(value)
    filename = get_file_name(filename)
    fig.savefig(filename)
    plt.close(fig)
    return filename

def get_summary(trains, groupby, months, muscle, exercise):
    values = prepare_data(trains, groupby, months, muscle, exercise)
    return [["date"] + list(values.keys())] + [
        [i.strftime('%Y-%m-%d')] + ["{:.0f}".format(r[k]) for k in values.keys()]
        for (i,r) in values.iterrows()]
    
def get_breakout(trains, value, period):
    values = prepare_breakout_data(trains, period)

    for k in values.keys():
        values[k] = values[k] / values.sum()[k]
    return [["muscle"] + list(values.keys())] + [
        [i] + ["{0:.0%}".format(r[k]) for k in values.keys()]
        for (i,r) in values.iterrows()]

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
