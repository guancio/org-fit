import tcxparser
from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np

filename = '/home/guancio/Sources/org-fit/activity_3016769645.tcx'
# filename = '/home/guancio/Sources/org-fit/activity_3029988752.tcx'

tcx = tcxparser.TCXParser(filename)

print """
* %s %s
  :PROPERTIES:
  :ID:             %s
  :type:           %s
  :date:           %s
  :time:           %s s
  :distance:       %s m
  :avg-pace:       %s mm:ss/km
  :avg-heart-rate: %s
  :max-heart-rate: %s
  :energy:         %s cal
  :elevation:      %s m
  :END:
[[%s]]
""" % (
    tcx.activity_type, tcx.time_values()[0],
    "",
    tcx.activity_type,
    tcx.time_values()[0],
    tcx.duration,
    tcx.distance,
    tcx.pace if tcx.distance != 0 else 0,
    tcx.hr_avg,
    tcx.hr_max,
    tcx.calories,
    tcx.ascent,
    filename)

# Overview
# - Elapsed time
# - VO2max
# - TRIMP

# Laps

# Heartrate data
print """
** Heartrate data
- avg. heart rate: %s (%.2f)
- max. heart rate: %s (%.2f)
- energy: %s cal
""" % (
    tcx.hr_avg,    tcx.hr_avg / 183.0 * 100,
    tcx.hr_max,    tcx.hr_max / 183.0 * 100,
    tcx.calories
    )

# TRIMP

# GRAPH
figname = filename[:-4] + ".png"
t1 = datetime.strptime(tcx.time_values()[0][:-1], "%Y-%m-%dT%H:%M:%S.%f")
times = [datetime.strptime(t[:-1], "%Y-%m-%dT%H:%M:%S.%f") for
         t in tcx.time_values()]
time_secs = [(t-t1).total_seconds() for t in times]
time_steps = [0]+[time_secs[i+1]-time_secs[i] for i in range(len(time_secs)-1)]

fig, ax = plt.subplots(nrows=1, ncols=1)
ax.plot(time_secs, tcx.hr_values())
fig.savefig(figname)
plt.title("Heartrate")
plt.legend()
# plt.show()
plt.close(fig)
print "[[%s]]" % figname

bins_hr = np.linspace(100, 180, 5)  # number of bins in the histograms

fig, ax = plt.subplots(nrows=1, ncols=1)
# ax.plot(time_secs, tcx.hr_values())
ax.hist(tcx.hr_values(), weights=time_steps, bins=bins_hr)
plt.title("Heartrate")
figname = filename[:-4] + "-histogram.png"
fig.savefig(figname)
# plt.show()
plt.close(fig)
print "[[%s]]" % figname
