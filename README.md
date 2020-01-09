# OLYMPUSWIFI
A desktop application to automate image download from Olympus cameras


MSGENDODOC

Hi everyone,

Since the Eye-Fi and Toshiba WiFi enabled SD cards have both been discontinued several years ago, there is no similar prodct now available on the market that i know of.  This dearth is presumably because most serious cameras now on the market have WiFi built in, so the commercial need for a WiFi SD card has reduced.  

I have an Olympus OM-D E-M10.  I need to use Windows at work to shoot images of clients. At this time I am too busy to take out the SD card and load the images onto the PC, as this would distract me from dealing with my clients. Therefore I really valued my previous Eye-fi card as it automated image download to PC.  The Eye-Fi card sensed when new images were taken and then signalled to the PC to change wireless networks to the Eye-Fi card, automatically download the images to PC and then revert the computers WiFi connection back to my work router.  Brilliant - until Eye-fi stopped supporting the software and went out of business.  Toshiba took over the intellectual property from Eye-Fi but I couldn't get a new Toshiba card to work at all, and this is now also defunct.  

Luckily my E-M10 has built in WiFi and can be connected to the Oishare olympus app in iOS or Android but I could not find a Windows version of Oishare.  I have therefore hacked the camera WiFi signal and found a way to read and download images from the camera to PC using some in house software I wrote, as I also do some coding.  I have open sourced this code and it can be downloaded from  https://github.com/MSGEndo/OLYMPUSWIFI  As I'm an older coder, the proejct it's written in Object Pascal using the Lazarus IDE (www.lazarus-ide.org), which a free open source coding language and free to use.  Please feel free to try my Windows cleint app for Olympus camera image downloads and it is useful to you and others.  Feel free to even modify and improve it, if you can code in Pascal (like your grandad!).  I have not been able to automatically swap my PC WiFi connection to and from the camera, so do need to manually change WiFi connections on the PC to link with the camera, but I can live with this.  

Hope this is helpful to olympus camera / PC users

Martin    

NOTES
How to use this program

  1.  Take some photos on your Olympus camera
  2.  Use the Olympus camera menu to select Connection to Smartphone.  This starts the camera WiFi server.
  3.  Connect your computer to this WiFi signal in the usual way for your operating system
  4.  Run this OlympusWIFI program
  5.  Set the download directory as required ("Transfer Files to This Directory") where  from the will be transferred
  6.  Option 1: Press the grey triangle once and wait for images on the SD card to be transferred to your computer's download directory
  7.  Option 2: Click the Repeat Timer checkbox and wait for the timer (e.g. 60s) to automate image transfers periodically.
  8.  Downloaded files will be remembered, so will not be downloaded again on later transfers.

  Enjoy

  Martin Gale ( 04/01/2020 Copyright.)
  mail@isendo.com.au
  This program is open source and can be used for commercial and non commercial purposes (MIT licence) so long as  full acknowledgement is made of the author's original contribution in any distribution of the derived work. The software is as is,  and no warranty of suitability of purpose or efficacy and is given, and the authors accept no liability whatsoever for its use, consequences or  effects.   If it is not suitable for your  purpose then do not use this software.  Otherwise, please make good use of it. 
