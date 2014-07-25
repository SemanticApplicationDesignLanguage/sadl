import os.path, time, difflib, filecmp
from filecmp import dircmp
def print_diff_files(dcmp):
    for name in dcmp.diff_files:
        fl = dcmp.left+"\\"+name
        fr = dcmp.right+"\\"+name
#        print ".left: %s" % (fl)
#        print ".right: %s" % (fr)
        print "diff: %s: (plug-in, repos)" % (name)
        print "  time: %s, %s" % (time.ctime(os.path.getmtime(fl)), \
            time.ctime(os.path.getmtime(fr)))
        sizel = os.stat(fl).st_size
        sizer = os.stat(fr).st_size
        if sizel != sizer:
            same = "(different!)"
        else:
            same = "(same)"
        print "  size: %s, %s %s" % (sizel, sizer, same)

#        fromlines = open(fl, 'U').readlines()
#        print "fromlines:******************"
#        print fromlines
#        tolines = open(fr, 'U').readlines()
#        print "tolines:************************"
#        print tolines
#        import pdb; pdb.set_trace()
#        for line in difflib.ndiff(fromlines, tolines):
#            print line
    for sub_dcmp in dcmp.subdirs.values():
        print_diff_files(sub_dcmp)

dcmp = dircmp(\
    'E:\crapo\workspaceSadlGEOnlyDistrib\com.ge.research.sadl.jena-plugin\src',\
    'E:\crapo\workspaceSadlGEOnlyDistrib\com.ge.research.sadl.jena-wrapper-for-sadl\src\main\java')
print "Comparing Jena SADL plugin (non-repo) with Open Source repo:"
print_diff_files(dcmp) 

dcmp = dircmp(\
    'E:\crapo\workspaceSadlGEOnlyDistrib\com.ge.research.sadl.jena-plugin\src',\
    'E:\crapo\workspaceSadlGEOnlyDistrib\com.ge.research.sadl.geonly.jena-wrapper-extensions\src\main\java') 
print "Comparing Jena SADL plugin (non-repo) with GE-Only OpenGE repo:"
print_diff_files(dcmp) 


