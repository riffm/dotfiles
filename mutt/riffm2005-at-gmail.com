set from = "riffm2005@gmail.com"
my_hdr Reply-To: $from
set sendmail = "msmtp -a riffm2005"
set spoolfile = "+riffm2005-at-gmail.com/INBOX"
set mbox = "+riffm2005-at-gmail.com/archive"
set postponed = "+riffm2005-at-gmail.com/drafts"
