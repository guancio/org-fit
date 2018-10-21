def import_csv(filein, fileout):
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

    out = open(fileout, "w")
    for workout in grouped_entry:
        out.write("""
* Workout %s""" % workout[0][0][0])
        for activity in workout:
            time = "\n:time: %s"%activity[0][7] if len(activity) == 1 and activity[0][7] != "" else ""
            distance = "\n:distance: %s %s"%(activity[0][5], activity[0][6]) if len(activity) == 1 and activity[0][5] != "" else ""
            out.write("""
** %s
:PROPERTIES:
:muscle: %s %s %s
:END:
""" % (activity[0][1], activity[0][2], time, distance))
            for rep in activity:
                if (rep[3] == "") and (rep[4] == ""):
                    continue
                if (float(rep[3]) > 0):
                    out.write("- %s kgs x %s reps\n" % (rep[3], rep[4]))
                else:
                    out.write("- %s reps\n" % (rep[4]))
    out.close()
    # :time: 0:05:00
