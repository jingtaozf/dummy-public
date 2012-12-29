#!/usr/bin/python
# Copyright (C) 2012 jingtao.net
# 
# Filename: trayicon-appt.py
# Description: show appt notification in linux systray
# Author: Xu Jingtao <jingtaozf@gmail.com>
# Created: 2012.12.03 21:40:39(+0800)
# Last-Updated: 2012.12.03 21:41:12(+0800)
#     Update #: 1
# 

# Commentary: 
# 
# 

import sys,os,threading,os.path,gtk,egg.trayicon,gobject
import Image

try:
    # egg == python-gnome2-extras
    import egg.trayicon
except:
    print "install the python-eggtrayicon package..."
    os.system("sudo emerge egg-python")

class EmacsIconTray:
    def __init__(self,timeout,tip_text):
        self.image_root="/home/projects/dummy/etc/"
        self.trayicon = egg.trayicon.TrayIcon("Emacs")
        self.eventbox = gtk.EventBox()
        self.trayicon.add(self.eventbox)
        self.pixbuf_blue = gtk.gdk.pixbuf_new_from_file(os.path.join (self.image_root, "emacs-blue-icon.png"))
        self.pixbuf_blue = self.pixbuf_blue.scale_simple(32, 32, gtk.gdk.INTERP_BILINEAR)
        self.pixbuf_red = gtk.gdk.pixbuf_new_from_file(os.path.join (self.image_root, "emacs-red-icon.png"))
        self.pixbuf_red = self.pixbuf_red.scale_simple(32, 32, gtk.gdk.INTERP_BILINEAR)
        self.current_pixbuf = self.pixbuf_red
        self.image = gtk.Image ()
        self.image.set_from_pixbuf (self.current_pixbuf)
        self.eventbox.add(self.image)

        self.tip_text = tip_text
  
        self.tip = gtk.Tooltips()
        self.tip.set_tip(self.eventbox, self.tip_text, tip_private=None)
        self.trayicon.show_all()
        self.eventbox.connect("button_press_event", gtk.main_quit)
        gobject.timeout_add_seconds(timeout,self.timer_callback)
    def timer_callback(self):
        if self.current_pixbuf == self.pixbuf_blue:
            self.current_pixbuf =  self.pixbuf_red
        else:
            self.current_pixbuf =  self.pixbuf_blue
        self.image.set_from_pixbuf (self.current_pixbuf)
        return True
def main (argv):
    tip_text = ""
    for arg in argv:
        tip_text = tip_text + arg
    emacs_icontray = EmacsIconTray(1,tip_text)
    gtk.main()
    
if __name__ == "__main__":
    main(sys.argv[1:])
