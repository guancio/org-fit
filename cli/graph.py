#!/usr/bin/python

import PyOrgMode
from datetime import datetime
import matplotlib.pyplot as plt
import argparse
from cmd import Cmd

class Rep():
    def __init__(self, weight, count):
        self.weight = weight if weight is not None else 0
        self.count = count


class Exercise():
    def __init__(self):
        self.reps = []


class Train():
    def __init__(self, p_date):
        self.date = p_date
        self.exercises = []


def clean_split(s,sep):
    return [v.strip() for v in s.split(sep)]

def parse_rep(s):
    if not s.startswith("-"):
        return None
    vals = [clean_split(v, " ") for v in clean_split(s[1:], "x")]
    return Rep(None if len(vals) < 2 else float(vals[0][0]), int(vals[-1][0]))

# a =  parse_rep("- 15.0 kgs x 10 reps\n")

def parse_ex(e2):
    """
Parse a complete exercise
    """
    ex = Exercise()
    for e3 in e2.content:
        if not isinstance(e3, str):
            continue
        res = parse_rep(e3)
        if res is None:
            continue
        ex.reps.append(res)
    return ex

# ex = parse_ex(e2)

def parse_tr(e1):
    time_text = e1.heading.split(" ")[1]
    time_value = datetime.strptime(time_text, "%Y-%m-%d")
    tr = Train(time_value)
    for e2 in e1.content:
        ex = parse_ex(e2)
        tr.exercises.append(ex)
    return tr

trains = []
def parse_gym_file(filename):
    global trains
    base = PyOrgMode.OrgDataStructure()
    base.load_from_file(filename)

    trains = []
    for e1 in base.root.content:
        if not isinstance(e1, PyOrgMode.OrgElement):
            continue
        trains.append(parse_tr(e1))
    return trains


def get_volume_train(tr):
    return sum([r.weight * r.count for e in tr.exercises for r in e.reps])
def get_sets_train(tr):
    return sum([len(e.reps) for e in tr.exercises])
def get_reps_train(tr):
    return sum([r.count for e in tr.exercises for r in e.reps])

def group_by_day(trains):
    res = []
    trs = []
    last_date = None
    for tr in trains:
        if tr.date == last_date:
            trs.append(tr)
            continue
        if len(trs) > 0:
            res.append((last_date, trs))
        last_date = tr.date
        trs = [tr]
    res.append((last_date, trs))
    return res

def group_by_month(trains):
    res = []
    trs = []
    month = None
    last_date = None
    for tr in trains:
        if tr.date.month == month:
            trs.append(tr)
            continue
        if len(trs) > 0:
            res.append((last_date, trs))
        month = tr.date.month
        last_date = tr.date
        trs = [tr]
    res.append((last_date, trs))
    return res

def group_by_week(trains):
    res = []
    trs = []
    week = None
    last_date = None
    for tr in trains:
        if tr.date.isocalendar()[1] == week:
            trs.append(tr)
            continue
        if len(trs) > 0:
            res.append((last_date, trs))
        week = tr.date.isocalendar()[1]
        last_date = tr.date
        trs = [tr]
    res.append((last_date, trs))
    return res

def filter_by_months(trains, months):
    if months is None:
        return trains
    t0 = datetime.now()
    return [tr for tr in trains if 
            tr.date.month >= t0.month - months
    ]


class GymGraphPrompt(Cmd):
    def do_load_org(self, args):
        """Load org file."""
        parse_gym_file(args)
        print "loaded %d sessions from %s" % (len(trains), args)


    def do_graph(self, args):
        """graph value groupby months"""
        (value,groupby,months,filename) = args.split(" ")
        months = None if months == "all" else int(months)
        local_trains = filter_by_months(trains, months)
        if groupby == "day":
            groups =  group_by_day(local_trains)
        elif groupby == "month":
            groups =  group_by_month(local_trains)
        elif groupby == "week":
            groups =  group_by_week(local_trains)
        volumes = [sum([get_volume_train(tr) for tr in trs]) for (d,trs) in groups]
        sets = [sum([get_sets_train(tr) for tr in trs]) for (d,trs) in groups]
        reps = [sum([get_reps_train(tr) for tr in trs]) for (d,trs) in groups]
        number = [len( trs) for (d,trs) in groups]
        dates = [d for (d, trs) in groups]

        if value == "vol":
            values = volumes
        elif value == "sets":
            values = sets
        elif value == "reps":
            values = reps
        elif value == "count":
            values = number

        fig, ax = plt.subplots(nrows=1, ncols=1)
        ax.plot(dates, values)
        plt.title(value)
        plt.legend()
        fig.savefig(filename)
        #plt.show()
        plt.close(fig)
        print "Produced %s" % filename

        
    def do_quit(self, args):
        """Quits the program."""
        print "Quitting."
        raise SystemExit

if __name__ == "__main__":
    prompt = GymGraphPrompt()
    prompt.prompt = '> '
    prompt.cmdloop('Starting prompt...')
