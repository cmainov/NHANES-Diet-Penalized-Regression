                                October 2006

 MyPyramid Equivalents Database for USDA Survey Food Codes, 1994-2002, Version 1.0
                              (MyPyrEquivDB_v1)
                                     


NOTE: For the best format to view and print this file, use Courier (10-point)
      and one inch margins.


The MyPyramid Equivalents Database for USDA Survey Food Codes, 1994-2002, 
Version 1.0 (MyPyrEquivDB_v1) was developed by the Community Nutrition Research 
Group (CNRG), Beltsville Human Nutrition Research Center (BHNRC), Agricultural
Research Service (ARS), U.S. Department of Agriculture (USDA).  The files in 
MyPyrEquivDB_v1 replace those in previous the servings databases released by 
CNRG. 

Five separate downloads are required to obtain all the files in MyPyrEquivDB_v1: 

  1.    MyPyramid equivalents food data ("fddata.exe") -- includes the number of 
        MyPyramid equivalents per 100 gram of food, food descriptions, data 
        formats, model programs, database documentation, and ReadMe.txt

  2-5.  MyPyramid equivalents intake data ("pyr_iff.exe", "pyr_tot.exe",
        "rt32.exe", "rt42.exe") -- each download includes one of the MyPyramid
        equivalents intake files, database documentation, and ReadMe.txt

The reason for separate downloads is that each MyPyramid equivalents intake data
file is large (2-23 MB).  When each of the 5 downloads is extracted, a directory 
"MyPyrEquivDB_v1" is created on your hard drive and files are placed into the 
directory as shown in the tree diagram (see end of Section 2 below).

The files "ReadMe.txt" and "doc.pdf" (the database documentation file) are
included in each download.  If more than one downloaded file is extracted,
"ReadMe.txt" and "doc.pdf" will be overwritten, not duplicated.


Information in this ReadMe file includes:

1. Introduction

2. Directories and files in the database


******************************************************************************
1.  Introduction
******************************************************************************

The MyPyramid Equivalents Database for USDA Survey Food Codes Version 1.0 provides 
equivalents data for use with national food consumption surveys conducted between 
1994 and 2002.  The equivalents in this database are based on the MyPyramid Food 
Guidance System which uses cups or ounces as the standard portion unit and 
identifies equivalent amounts of those measures for commonly consumed foods.  
The MyPyramid Food Guidance System refers to it’s defined portion units as “ounce 
equivalents” or “cup equivalents”.  
                   
This database provides equivalents data by five major food groups (grains, 
vegetables, fruits, meat and beans, and milk) and selected subgroups, as well as 
amounts of oil, solid fats, added sugars, and alcohol (32 groups total).  There are two 
types of MyPyramid equivalents data files:
    -   MyPyramid equivalents food data 
    -   MyPyramid equivalents intake data

Other files in the MyPyramid equivalents database include documentation of the 
methods used to classify foods and assign equivalent amounts, food code descriptions, 
file formats, and model programs for joining the MyPyramid equivalents food data files 
to survey food intake records and for comparing equivalents intakes to 
recommendations.

MyPyrEquivDB_v1 includes MyPyramid equivalents intakes for all individuals 2 years and
older from: 
    -  What We Eat in America, the dietary interview component of the 
       National Health and Nutrition Examination Survey (NHANES) 2001-2002 
    -  NHANES 1999-2000 
    -  Continuing Survey of Food Intakes of Individuals (CSFII) 1994-96, 1998

Users should read the database documentation ("doc.pdf") before using the 
MyPyramid equivalents intake data, especially Section 4 on data file 
characteristics and formats and Section 5 on issues related to using the 
MyPyramid intake files.  It is important to read the statistical guidelines for 
using the MyPyramid intake files (Section 5.3).

The MyPyramid Equivalents Database can also be used with any survey conducted 
between 1994 and 2002 that uses USDA survey food codes.


******************************************************************************
2.  Directories and files in the database                  C:\MyPyrEquivDB_v1
******************************************************************************

The directories and files created on the computer's hard drive when the 
database files are downloaded and then extracted are shown below.


C:\MyPyrEquivDB_v1\
       |  
       |--data\                      Data files and formats
       |    |
       |    |--equiv0102\            MyPyramid equivalents food data for USDA food
       |    |    |                   codes used to process What We Eat in 
       |    |    |                   America, NHANES 2001-2002
       |    |    |
       |    |    |-equiv0102.txt     Number of equivalents for each of the 32
       |    |    |                   Pyramid food groups per 100 grams food 
       |    |    |                   code (ASCII fixed format)
       |    |    | 
       |    |    |-fddes0102.txt     Food code descriptions (ASCII fixed
       |    |                        format)
       |    |
       |    |
       |    |-equiv9400\             MyPyramid equivalents food data for USDA food
       |    |    |                   codes used to process CSFII 1994-96, 1998
       |    |    |                   and NHANES 1999-2000
       |    |    |
       |    |    |-equiv9400.txt     Number of equivalents for each of the 32  
       |    |    |                   Pyramid food groups per 100 grams food 
       |    |    |                   code (ASCII fixed format)
       |    |    |                   
       |    |    |-fddes9400.txt     Food code descriptions (ASCII fixed 
       |    |                        format)
       |    |
       |    | 
       |    |--formats\              File formats for MyPyramid food data files
       |    |    |
       |    |    |-f_equiv.txt       Format for the equiv*.txt files (ASCII 
       |    |    |                   fixed format)
       |    |    |
       |    |    |-f_fddes.txt       Format for the fddes*.txt files (ASCII 
       |    |                        fixed format) 
       |    |
       |    |   
       |    |--intakes\              MyPyramid equivalents intake data files
       |         |
       |         |-pyr_iff.sas7bdat  Number of MyPyramid equivalents provided by
       |         |                   each food eaten by each individual in 
       |         |                   What We Eat in America, NHANES 2001-2002 
       |         |                   and NHANES 1999-2000 (SAS format)
       |         | 
       |         |-pyr_tot.sas7bdat  Daily total MyPyramid equivalents intakes per
       |         |                   individual for What We Eat in America, 
       |         |                   NHANES 2001-2002 and NHANES 1999-2000
       |         |                   (SAS format)
       |         |
       |         |-rt32.sas7bdat     Number of MyPyramid equivalents provided by
       |         |                   each food eaten by each individual in 
       |         |                   CSFII 1994-96, 1998 (SAS format)
       |         | 
       |         |-rt42.sas7bdat     Daily total MyPyramid equivalents intakes per
       |                             individual in CSFII 1994-96, 1998;
       |                             includes Day 1, Day 2, 2-day average for
       |                             individuals with 2 days of intake 
       |                             (SAS format)
       |           
       |--doc\                       Documentation 
       |    |
       |    |-doc.pdf                Documentation of MyPyrEquivDB_v1
       |                                Section 1.  Table of Contents
       |                                Section 2.  Essential Information
       |                                Section 3.  Methodology: Development
       |                                            of the MyPyramid equivalents 
       |                                            Database 
       |                                Section 4.  Data File Characteristics
       |                                            and Formats
       |                                Section 5.  Using the MyPyramid equivalents 
       |                                            Food and Intake Files
       |                                Appendix A. Useful Web Sites
       |                                Appendix B. What counts in each MyPyramid
       |                                            Food Group
       |                                Appendix C. Control counts for MyPyramid
       |                                            equivalents food and intake 
       |                                            data files 
       |                                Appendix D. Program files
       |
       |--programs\                  Model SAS(R) programs to use 
       |   |                         MyPyrEquivDB_v1 with dietary survey food
       |   |                         intake data (see Section 5.2)
       |   | 
       |   |-pyr_iff.sas             Joins the MyPyramid food data 
       |   |                         "equiv0102.sas7bdat" to What We Eat in
       |   |                         Amercia, NHANES 2001-2002 food intake and
       |   |                         demographic records and 
       |   |                         "equiv9400.sas7bdat" to NHANES 1999-2000 
       |   |                         food intake and demographic records to 
       |   |                         create the combined NHANES 1999-2002 
       |   |                         MyPyramid equivalents intake data provided by 
       |   |                         each food eaten per individual 2 years 
       |   |                         and older ("pyr_iff.sas7bdat")
       |   |
       |   |-pyr_tot.sas             Reads NHANES 1999-2002 MyPyramid food 
       |   |                         intake data from "pyr_iff.sas7bdat" and
       |   |                         creates daily MyPyramid equivalents intake data
       |   |                         ("pyr_tot.sas7bdat") per individuals two 
       |   |                         years and older
       |   |
       |   |-pyrrpt.sas              Use to prepare MyPyramid equivalents intake 
       |   |                         estimates; 
       |   |
       |   |-pyrrecom.sas            Use to prepare MyPyramid equivalents intake
       |   |                         estimates and compare equivalents to 
       |   |                         recommendations
       |   |
       |   |-readfsrv.sas            Reads "equiv0102.txt" and "equiv9400.txt";
       |   |                         stores data in SAS formats
       |   |                         ("equiv0102.sas7bdat" and 
       |   |                         "equiv9400.sas7bdat")
       |   |
       |   |-rt32.sas                Joins "equiv9400.sas7bdat" food data file 
       |   |                         to CSFII 1994-96, 1998 intake and 
       |   |                         demographic data files to create MyPyramid 
       |   |                         equivalents intake data provided by each food
       |   |                         eaten per individual 2 years and older
       |   |                         ("rt32.sas7bdat")
       |   |
       |   |- rt42.sas               Reads CSFII 1994-96, 1998 MyPyramid food
       |                             intake data from "rt32.sas7bdat" and 
       |                             creates daily MyPyramid equivalents intake data
       |                             ("rt42.sas7bdat") per individual for 
       |                             Day 1, Day 2,and a 2-day average for 
       |                             individuals completing intakes for 2 days
       |
       |--ReadMe.txt                 Introduction and directory structure for 
                                     MyPyrEquivDB_v3 

           

******************************************************************************
*                                                                            *
*   Commercial Endorsement Disclaimer:                                       *
*                                                                            *
*     The use of trade, firm, or corporation names in this database is       *
*     for the information and convenience of the reader.  Such use does      *
*     not constitute an official endorsement or approval by the United       *
*     States Department of Agriculture or the Agricultural Research          *
*     Service of any product or service to the exclusion of others that      *
*     may be suitable.                                                       *
*                                                                            *
******************************************************************************



******************************************************************************
*                                                                            *
*   USDA Nondiscrimination Statement:                                        *
*                                                                            *
*     The U.S. Department of Agriculture (USDA) prohibits discrimination     *
*     in all its programs and activities on the basis of race, color,        *
*     national origin, sex, religion, age, disability, political beliefs,    *
*     sexual orientation, or marital or family status.  (Not all             *
*     prohibited bases apply to all programs.)  Persons with disabilities    *
*     who require alternative means for communication of program             *
*     information (Braille, large print, audiotape, etc.) should contact     *
*     USDA's TARGET Center at (202) 720-2600 (voice and TDD).  To file a     *
*     complaint of discrimination, write USDA, Office of Civil Rights,       *
*     Room 326-W, Whitten Building, 1400 Independence Avenue, SW,            *
*     Washington, D.C.  20250-9410 or call (202) 720-5964 (voice and TDD).   *
*     USDA is an equal opportunity provider and employer.                    *
*                                                                            *
******************************************************************************



