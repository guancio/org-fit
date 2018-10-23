#!/usr/bin/python

import orgparser
import graph
import csvimport
from cmd import Cmd

class FitCli(Cmd):
    trains = None
    def do_load_org(self, args):
        """Load org file."""
        self.trains = orgparser.parse_gym_file(args)
        print "loaded %d sessions from %s" % (len(self.trains), args)


    def do_graph(self, args):
        """graph value groupby months"""
        (value,groupby,months,muscle,filename) = args.split(" ")
        months = None if months == "all" else int(months)
        muscle = None if muscle == "all" else muscle

        graph.draw_line_graph(self.trains, value, groupby, months, muscle, filename)
        print "Produced %s" % filename

    def do_pie(self, args):
        """pie value groupby muscle"""
        (value,period,filename) = args.split(" ")

        graph.draw_pie_graph(self.trains, value, period, filename)
        print "Produced %s" % filename

    def do_list_muscles(self, args):
        r = graph.get_all_muscles(self.trains)
        print ",".join(r.index.values)
        
    def do_import (self, args):
        (filein, fileout) = args.split(" ")
        csvimport.import_csv(filein, fileout)
        print "Produced %s" % fileout

    def do_quit(self, args):
        """Quits the program."""
        print "Quitting."
        raise SystemExit

if __name__ == "__main__":
    prompt = FitCli()
    prompt.prompt = '> '
    prompt.cmdloop('Starting prompt...')
