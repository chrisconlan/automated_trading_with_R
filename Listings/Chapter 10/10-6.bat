schtasks /create /tn PLAN /sc weekly /d mon,tue,wed,thu,fri /mo 1 /st 19:00
  /tr "C:\Platform\plan.bat"
