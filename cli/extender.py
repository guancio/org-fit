import pandas as pd
import numpy as np
import math
from datetime import datetime
import time
import PyOrgMode
import orgparser


    

def extend_epley(element):
    properties = orgparser.get_property_drawer(element)
    epley_p = orgparser.get_property(properties, "epley")
    if epley_p is not None:
        return
    values = orgparser.parse_ex(element)
    values['reps'] = values['count']
    values['epley'] = values['weight'] * (1 + values['reps']/30)
    epley =  values['epley'].max()
    if math.isnan(epley):
        return
    properties.append(PyOrgMode.OrgDrawer.Property("epley", "%d kg" % epley))


def extend_speed(element):
    properties = orgparser.get_property_drawer(element)
    distance = orgparser.get_property(properties, "distance")
    if distance is None:
        return
    distance = float(distance.value.split(" ")[0])
    if distance == 0:
        return
    duration = orgparser.get_property(properties, "duration")
    if duration is None:
        return
    duration = time.strptime(duration.value.strip(), "%H:%M:%S")
    duration = duration.tm_hour * 3600 + duration.tm_min * 60 + duration.tm_sec
    if orgparser.get_property(properties, "speed") is None:
        properties.append(PyOrgMode.OrgDrawer.Property("speed", "%.2f km/h" % (distance * 3.6 / 1000)))
    if orgparser.get_property(properties, "pace") is None:
        properties.append(PyOrgMode.OrgDrawer.Property("pace", "%d:%d m/km" %
                                                       ((duration/(distance / 1000)/60),
                                                        (duration/(distance / 1000) % 60)
                                                        )))
def extend_exercise(element):
    extend_epley(element)
    extend_speed(element)
    
def extend_workout(workout):
    for e2 in workout.content:
        if not isinstance(e2, PyOrgMode.OrgElement):
            continue
        if e2.TYPE == "DRAWER_ELEMENT":
            continue
        if e2.TYPE == "TABLE_ELEMENT":
            continue
        extend_exercise(e2)

def extend_db(db):
    for e in db.content:
        if not isinstance(e, PyOrgMode.OrgElement):
            continue
        extend_workout(e)

def extend_gym_file(filename):
    base = PyOrgMode.OrgDataStructure()
    base.load_from_file(filename)

    extend_db(base.root)
    base.save_to_file(filename)

# extend_gym_file("/home/guancio/Sources/org-fit/data/res.org")
