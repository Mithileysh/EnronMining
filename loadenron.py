docs = []
from os import listdir, chdir
import re


# Filter out useless enron emails parts
email_pat = re.compile(".+@.+")
to_pat    = re.compile("To:.+\n")
cc_pat    = re.compile("cc:.+\n")
subject_pat = re.compile("Subject:.+\n")
from_pat  = re.compile("From:.+\n")
sent_pat  = re.compile("Sent:.+\n")
received_pat = re.compile("Received:.+\n")
ctype_pat = re.compile("Content-Type:.+\n")
reply_pat = re.compile("Reply- Organization:.+\n")
date_pat  = re.compile("Date:.+\n")
xmail_pat = re.compile("X-Mailer:.+\n")
mimver_pat = re.compile("MIME-Version:.+\n")
contentinfo_pat = re.compile("----------------------------------------.+----------------------------------------")
forwardedby_pat = re.compile("----------------------.+----------------------")
caution_pat = re.compile('''\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*.+\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*''')
privacy_pat = re.compile(" _______________________________________________________________.+ _______________________________________________________________")


# Make one long list of the email
chdir("C:/Users/andry/Desktop/KernixChallenge/maildirsample")
names = [d for d in listdir(".") if "." not in d]
for name in names:
    chdir("C:/Users/andry/Desktop/KernixChallenge/maildirsample/%s" % name)
    subfolders = listdir('.')
    sent_dirs = [n for n, sf in enumerate(subfolders) if "sent" in sf]
    sent_dirs_words = [subfolders[i] for i in sent_dirs]
    for d in sent_dirs_words:
        chdir('C:/Users/andry/Desktop/KernixChallenge/maildirsample/%s/%s' % (name,d))
        file_list = listdir('.')
        print(file_list)
        docs.append([" ".join(open(f, 'r').readlines()) for f in file_list])


# For each email : filter / clean and make of flat cleaned list
docs_final = []
for subfolder in docs:
    for email in subfolder:
        if ".nsf" in email:
            etype = ".nsf"
        elif ".pst" in email:
            etype = ".pst"
        email_new = email[email.find(etype)+4:]
        email_new = to_pat.sub('', email_new)
        email_new = cc_pat.sub('', email_new)
        email_new = subject_pat.sub('', email_new)
        email_new = from_pat.sub('', email_new)
        email_new = sent_pat.sub('', email_new)
        email_new = email_pat.sub('', email_new)
        if "-----Original Message-----" in email_new:
            email_new = email_new.replace("-----Original Message-----","")
        email_new = ctype_pat.sub('', email_new)
        email_new = reply_pat.sub('', email_new)
        email_new = date_pat.sub('', email_new)
        email_new = xmail_pat.sub('', email_new)
        email_new = mimver_pat.sub('', email_new)
        email_new = contentinfo_pat.sub('', email_new)
        email_new = forwardedby_pat.sub('', email_new)
        email_new = caution_pat.sub('', email_new)
        email_new = privacy_pat.sub('', email_new)
        docs_final.append(email_new)


# Dump each cleaned email in separate files
for n, doc in enumerate(docs_final):
    print(n)
    outfile = open("C:/Users/andry/Desktop/KernixChallenge/maildirsample/clsdata/%s.txt" % n,'w')
    outfile.write(doc)
    outfile.close()
