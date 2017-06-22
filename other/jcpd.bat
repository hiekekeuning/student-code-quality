@set TOPDIR=%~dp0
@set OPTS=
@set MAIN_CLASS=spa.CPDRunner

java -cp "%TOPDIR%\bin;%TOPDIR%\lib\*" %OPTS% %MAIN_CLASS% %*