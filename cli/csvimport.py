import pandas as pd
import numpy as np
import math
from datetime import datetime
import time
import PyOrgMode
import orgparser

def import_csv(filein, fileout):
    base = PyOrgMode.OrgDataStructure()
    base.load_from_file(fileout)
    lines = open(filein).read()
    lines = lines.split("\n")[1:]
    lines = [l.split(",") for l in lines]
    last_time = None
    date_entries = []
    date_entry = None
    for l in lines:
        if l[0] != last_time:
            last_time = l[0]
            if date_entry is not None:
                date_entries.append(date_entry)
            date_entry = []
        date_entry.append(l)
     
    if date_entry is not None:
        date_entries.append(date_entry)
     
    grouped_entry = []
    for entry in date_entries:
        last_activity = None
        activities = []
        activity = None
        for l in entry:
            if l[1] != last_activity:
                last_activity = l[1]
                if activity is not None:
                    activities.append(activity)
                activity = []
            activity.append(l)
        if activity is not None:
            activities.append(activity)
        grouped_entry.append(activities)

    for workout in grouped_entry:
        new_workout = PyOrgMode.OrgNode.Element()
        new_workout.heading = "Workout"
        new_workout.level = 1

        workout_props = PyOrgMode.OrgDrawer.Element("PROPERTIES")
        workout_props.append(PyOrgMode.OrgDrawer.Property("date", workout[0][0][0]))
        new_workout.append_clean(workout_props)

        for activity in workout:
            new_activity = PyOrgMode.OrgNode.Element()
            new_activity.heading = activity[0][1]
            new_activity.level = 2
            activity_props = PyOrgMode.OrgDrawer.Element("PROPERTIES")
            activity_props.append(PyOrgMode.OrgDrawer.Property("muscle", activity[0][2]))
            if len(activity) == 1 and activity[0][7] != "":
                activity_props.append(PyOrgMode.OrgDrawer.Property("duration", activity[0][7]))
            if len(activity) == 1 and activity[0][5] != "":
                activity_props.append(PyOrgMode.OrgDrawer.Property(
                    "distance",
                    "%s %s"%(activity[0][5], activity[0][6])))
            new_activity.append_clean(activity_props)
            
            for rep in activity:
                if (rep[3] == "") and (rep[4] == ""):
                    continue
                if (float(rep[3]) > 0):
                    new_activity.append_clean("- %s kgs x %s reps\n" % (rep[3], rep[4]))
                else:
                    new_activity.append_clean("- %s reps\n" % (rep[4]))
            new_workout.append_clean(new_activity)
        base.root.append_clean(new_workout)
    base.save_to_file(fileout)
