import PyOrgMode
from datetime import datetime
import time
import pandas as pd

def get_property_drawer(element):
    for e in element.content:
        if not isinstance(e, PyOrgMode.OrgElement):
            continue
        if e.TYPE == "DRAWER_ELEMENT" or e.name == "PROPERTIES":
            return e
    return None

def get_property(element, name):
    if element is None:
        return None
    p = [p for p in element.content if p.name == name]
    if len(p) == 0:
        return None
    return p[0]

def get_property_value(element, name):
    p = get_property(element, name)
    if p is None:
        return None
    return p.value.strip()

def get_content_strings(element):
    return [e for e in element.content if isinstance(e, str)]

def get_content_nodes(element):
    return [e for e in element.content if isinstance(e, PyOrgMode.OrgElement) and
            e.TYPE != "DRAWER_ELEMENT"]

                
def clean_split(s,sep):
    return [v.strip() for v in s.split(sep)]

def parse_rep(s):
    if not s.startswith("-"):
        return None
    vals = [clean_split(v, " ") for v in clean_split(s[1:], "x")]
    return {'weight' : None if len(vals) < 2 else float(vals[0][0]),
            'count': int(vals[-1][0]),
            'distance': None,
            'duration': None
    }

#  parse_rep("- 15.0 kgs x 10 reps\n")

def parse_ex(e2):
    """
Parse a complete exercise
    """
    ex = pd.DataFrame({ 'weight' : [], 'count': [], 'distance':[], 'duration':[], 'muscle':[]})
    properties = get_property_drawer(e2)
    muscle = get_property_value(properties, "muscle")
    duration = get_property_value(properties, "duration")
    if duration is not None:
        duration = time.strptime(duration, "%H:%M:%S")
        duration = duration.tm_hour * 3600 + duration.tm_min * 60 + duration.tm_sec
    distance = get_property_value(properties, "distance")
    if distance is not None:
        distance = distance.split(" ")
        if distance[1] == "km":
            distance = float(distance[0]) * 1000
        else:
            distance = float(distance[0])
    for e3 in get_content_strings(e2):
        res = parse_rep(e3)
        if res is None:
            continue
        ex = ex.append(res, ignore_index=True)
    
    if len(ex) == 0:
        ex = ex.append({"weight" : None, "count": 1, "duration": duration, "distance": distance}, ignore_index=True)
    ex['muscle'] = muscle
    ex['exercise'] = e2.heading
    return ex

# parse_ex(base.root.content[1].content[1])

def parse_tr(e1):
    tr = pd.DataFrame({ 'date': [], 'weight' : [], 'count': [], 'muscle' : [], 'distance':[], 'duration':[]})
    properties = get_property_drawer(e1)
    time_value = datetime.strptime(get_property_value(properties, "date"), "%Y-%m-%d")
    for e2 in get_content_nodes(e1):
        ex = parse_ex(e2)
        ex['date'] = time_value
        tr = tr.append(ex, ignore_index=True)
    return tr

# parse_tr(base.root.content[1])

def parse_gym_file(filename):
    base = PyOrgMode.OrgDataStructure()
    base.load_from_file(filename)

    trains = pd.DataFrame({ 'date': [], 'weight' : [], 'count': [], 'distance': [], 'duration':[]})
    for e1 in get_content_nodes(base.root):
        trains = trains.append(parse_tr(e1), ignore_index=True)
    return trains

# parse_gym_file("/home/guancio/Sources/org-fit/data/res.org")
