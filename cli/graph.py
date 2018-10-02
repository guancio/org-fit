#!/usr/bin/python

import PyOrgMode
from datetime import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from cmd import Cmd

def clean_split(s,sep):
    return [v.strip() for v in s.split(sep)]

def parse_rep(s):
    if not s.startswith("-"):
        return None
    vals = [clean_split(v, " ") for v in clean_split(s[1:], "x")]
    return {'weight' : None if len(vals) < 2 else float(vals[0][0]),
            'count': int(vals[-1][0])}

#  parse_rep("- 15.0 kgs x 10 reps\n")

def parse_ex(e2):
    """
Parse a complete exercise
    """
    ex = pd.DataFrame({ 'weight' : [], 'count': []})
    muscle = None
    for e3 in e2.content:
        if isinstance(e3, PyOrgMode.OrgElement):
            if e3.name != "PROPERTIES":
                continue
            for prop in e3.content:
                if prop.name == "muscle":
                    muscle = prop.value.strip()
            continue
        if not isinstance(e3, str):
            continue
        res = parse_rep(e3)
        if res is None:
            continue
        ex = ex.append(res, ignore_index=True)
    ex['muscle'] = muscle
    return ex

# parse_ex(base.root.content[1].content[1])

def parse_tr(e1):
    tr = pd.DataFrame({ 'date': [], 'weight' : [], 'count': [], 'muscle' : []})
    time_text = e1.heading.split(" ")[1]
    time_value = datetime.strptime(time_text, "%Y-%m-%d")
    for e2 in e1.content:
        ex = parse_ex(e2)
        ex['date'] = time_value
        tr = tr.append(ex, ignore_index=True)
    return tr

# parse_tr(base.root.content[1])

trains = pd.DataFrame({ 'date': [], 'weight' : [], 'count': []})
def parse_gym_file(filename):
    global trains
    base = PyOrgMode.OrgDataStructure()
    base.load_from_file(filename)

    trains = pd.DataFrame({ 'date': [], 'weight' : [], 'count': []})
    for e1 in base.root.content:
        if not isinstance(e1, PyOrgMode.OrgElement):
            continue
        trains = trains.append(parse_tr(e1), ignore_index=True)
    return trains

# parse_gym_file("/home/guancio/Sources/org-fit/data/res.org")

def prepare_data(value, groupby, months, muscle):
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
    grouped = to_process.groupby(pd.Grouper(freq=group_freq))
    if value == "sets":
        values = grouped.size()
    elif value == "reps":
        values = grouped.agg({'count': np.sum})
    elif value == "vol":
        values = grouped.agg({'volume': np.sum})
    # elif value == "count":
    #     values = grouped
    else:
        return None
    #some bugs with volume
    values = values[values > 0]
    return values

def get_all_muscles():
    return trains.groupby('muscle').count()

# trains.groupby('muscle').count()
# trains.groupby('muscle').count().plot.pie(subplots=True)
# trains.groupby('muscle').count()['count'].plot.pie()

class GymGraphPrompt(Cmd):
    def do_load_org(self, args):
        """Load org file."""
        parse_gym_file(args)
        print "loaded %d sessions from %s" % (len(trains), args)


    def do_graph(self, args):
        """graph value groupby months"""
        (value,groupby,months,muscle,filename) = args.split(" ")
        months = None if months == "all" else int(months)
        muscle = None if muscle == "all" else muscle

        to_plot = prepare_data(value, groupby, months, muscle)
        if to_plot is None:
            return
        fig, ax = plt.subplots(nrows=1, ncols=1)
        ax.plot(to_plot)
        plt.title(value)
        plt.legend()
        fig.savefig(filename)
        #plt.show()
        plt.close(fig)
        print "Produced %s" % filename

    def do_list_muscles(self, args):
        r = get_all_muscles()
        print ",".join(r.index.values)
        
    def do_quit(self, args):
        """Quits the program."""
        print "Quitting."
        raise SystemExit

if __name__ == "__main__":
    prompt = GymGraphPrompt()
    prompt.prompt = '> '
    prompt.cmdloop('Starting prompt...')
