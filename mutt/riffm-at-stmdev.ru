set from = "riffm@stmdev.ru"
my_hdr Reply-To: $from
my_hdr Bcc: $from
set sendmail = "msmtp -a stmdev"
set spoolfile = "+riffm-at-stmdev.ru/INBOX"
set mbox = "+riffm-at-stmdev.ru/archive"
set postponed = "+riffm-at-stmdev.ru/drafts"
