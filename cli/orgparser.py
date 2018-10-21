import PyOrgMode
from datetime import datetime
import pandas as pd

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

def parse_gym_file(filename):
    base = PyOrgMode.OrgDataStructure()
    base.load_from_file(filename)

    trains = pd.DataFrame({ 'date': [], 'weight' : [], 'count': []})
    for e1 in base.root.content:
        if not isinstance(e1, PyOrgMode.OrgElement):
            continue
        trains = trains.append(parse_tr(e1), ignore_index=True)
    return trains

# parse_gym_file("/home/guancio/Sources/org-fit/data/res.org")
