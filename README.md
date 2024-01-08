# ACS_SES
#R file to download and process socioeconomic data from 5yr (2005-2009, 2010-2014, and 2014-2018) ACS data files

#Variable cleaning: https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
sum = x + y + z
moe.sum = sqrt(x^2 + y^2 + z2)
proportion = x/y
moc.proportion = 1/y * sqrt((moe.x)^2 - [x/y * (moe.y)^2])
^Users should note that if the value under the square root is negative, then the formula for calculating the MOE of a ratio should be used instead, which substitutes a “plus” for the “minus” under the square root.

#Variables used:
acs18_vars <-c(tot_pop='B01001_001',tot_pop_racetab='B03002_001',Hispanic = "B03002_012", White = "B03002_003", Black = "B03002_004", Asian = "B03002_006", 
                     MHI='B19013_001', tot_pop_FPL='B17026_001', FPL00.50='B17026_002', FPL50.74='B17026_003',FPL75.99='B17026_004', FPL100.124='B17026_005', 
                     FPL125.149='B17026_006', FPL150.184='B17026_007', FPL185.199='B17026_008', gini='B19083_001',
                     educ.pop_ge_25='B15003_001',educ.none='B15003_002', educ.prek='B15003_003', educ.kind='B15003_004', educ.1st='B15003_005', educ.2nd='B15003_006', 
                     educ.3rd='B15003_007', educ.4th='B15003_008', educ.5th='B15003_009', educ.6th='B15003_010', educ.7th='B15003_011', educ.8th='B15003_012',
                     educ.9th='B15003_013', educ.10th='B15003_014', educ.11th='B15003_015', educ.12th_nodegree='B15003_016',educ.HS='B15003_017', 
                     educ.ged='B15003_018', educ.some_coll1yr='B15003_019', educ.some_coll='B15003_020', educ.assocdeg='B15003_021',
                     educ.bach='B15003_022', educ.mast='B15003_023', educ.prof='B15003_024', educ.doct='B15003_025',
                     emp.pop_ge_16='B23025_001',  emp.civlabforce='B23025_003',  emp.unemp_civlabforce='B23025_005')

acs14_vars <-c(tot_pop='B01003_001',tot_pop_racetab='B03002_001',Hispanic = "B03002_012", White = "B03002_003", Black = "B03002_004", Asian = "B03002_006", 
               MHI='B19013_001', tot_pop_FPL='C17002_001', FPL00.50='C17002_002', FPL50.99='C17002_003', FPL100.124='C17002_004', 
               FPL125.149='C17002_005', FPL150.184='C17002_006', FPL185.199='C17002_007', gini='B19083_001',
               educ.pop_ge_25='B15003_001',educ.none='B15003_002', educ.prek='B15003_003', educ.kind='B15003_004', educ.1st='B15003_005', educ.2nd='B15003_006', 
               educ.3rd='B15003_007', educ.4th='B15003_008', educ.5th='B15003_009', educ.6th='B15003_010', educ.7th='B15003_011', educ.8th='B15003_012',
               educ.9th='B15003_013', educ.10th='B15003_014', educ.11th='B15003_015', educ.12th_nodegree='B15003_016',educ.HS='B15003_017', 
               educ.ged='B15003_018', educ.some_coll1yr='B15003_019', educ.some_coll='B15003_020', educ.assocdeg='B15003_021',
               educ.bach='B15003_022', educ.mast='B15003_023', educ.prof='B15003_024', educ.doct='B15003_025',
               emp.pop_ge_16='B23025_001',  emp.civlabforce='B23025_003',  emp.unemp_civlabforce='B23025_005')

key_acs_vars2009 <-c(tot_pop='B01003_001',tot_pop_racetab='B03002_001',Hispanic = "B03002_012", White = "B03002_003", Black = "B03002_004", Asian = "B03002_006", 
                     MHI='B19013_001', tot_pop_FPL='C17002_001',FPL00.50='C17002_002', FPL50.99='C17002_003', FPL100.124='C17002_004', 
                     FPL125.149='C17002_005', FPL150.184='C17002_006', FPL185.199='C17002_007', #gini='B19083_001',
                     emp.pop_ge_16='C23002A_001',unemp16.64men='C23002A_008',  unemp16.64women='C23002A_021',unemp65men='C23002A_013',  unemp65women='C23002A_026',
                     educ.pop_ge_25='B15002_001',educ.male.pop_ge_25='B15002_002',educ.male.none='B15002_003', educ.male.lt5th='B15002_004', educ.male.56th='B15002_005', 
                     educ.male.78th='B15002_006', educ.male.9th='B15002_007', educ.male.10th='B15002_008', educ.male.11th='B15002_009', educ.male.12th_nodegree='B15002_010',
                     educ.male.HSged='B15002_011', educ.male.some_coll1yr='B15002_012', educ.male.some_coll='B15002_013', educ.male.assocdeg='B15002_014',
                     educ.male.bach='B15002_015', educ.male.mast='B15002_016', educ.male.prof='B15002_017', educ.male.doct='B15002_018',
                     educ.female.pop_ge_25='B15002_019',educ.female.none='B15002_020', educ.female.lt5th='B15002_021', educ.female.56th='B15002_022', 
                     educ.female.78th='B15002_023', educ.female.9th='B15002_024', educ.female.10th='B15002_025', educ.female.11th='B15002_026', educ.female.12th_nodegree='B15002_027',
                     educ.female.HSged='B15002_028', educ.female.some_coll1yr='B15002_029', educ.female.some_coll='B15002_030', educ.female.assocdeg='B15002_031',
                     educ.female.bach='B15002_032', educ.female.mast='B15002_033', educ.female.prof='B15002_034', educ.female.doct='B15002_035')
