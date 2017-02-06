#install.packages('XML',dep=T);
library('XML');
xmldoc = xmlTreeParse('NSFExample.xml');
root = xmlRoot(xmldoc);
node1 = root[[1]];
node1 = xmlChildren(root)$Award
node11 = xmlChildren(node1)$AwardTitle
xmlValue(node11)
###All children of node1

xmlChildren(node1)
#############Processing the file
lst_chld = xmlChildren(node1);
V1 = xmlValue(lst_chld$AwardTitle);
V2 = xmlValue(lst_chld$AwardEffectiveDate);
V3 = xmlValue(lst_chld$AwardExpirationDate);
V4 = xmlValue(lst_chld$AwardAmount);
V5 = paste(xmlValue(xmlChildren(lst_chld$Investigator)$FirstName), xmlValue(xmlChildren(lst_chld$Investigator)$LastName), sep =
             " ")
V6 = xmlValue(xmlChildren(lst_chld$Institution)$Name)

c(V1,V2,V3,V4,V5,V6)
#############Processing all the NSF files

xmlfun = function(node) {
  lst_chld = xmlChildren(node);
  V1 = xmlValue(lst_chld$AwardTitle);
  V2 = xmlValue(lst_chld$AwardEffectiveDate);
  V3 = xmlValue(lst_chld$AwardExpirationDate);
  V4 = xmlValue(lst_chld$AwardAmount);
  V5 = paste(xmlValue(xmlChildren(lst_chld$Investigator)$FirstName), xmlValue(xmlChildren(lst_chld$Investigator)$LastName), sep =
               " ")
  V6 = xmlValue(xmlChildren(lst_chld$Institution)$Name)
  return(c(V1,V2,V3,V4,V5,V6))
}
##Using xmlfun
doc = xmlTreeParse('NSFExample.xml');
root = xmlRoot(doc)
node_Award = root[[1]]
xmlfun(node_Award)

##Processing all the files
Files = system('ls ./2014',intern = T);
L = length(Files)
## Prepare the dataset
dt = data.frame(matrix(NA,nrow = L,ncol = 6));
names(dt) = c('Title','date_start','date_end','Amount','PI_name','PI_Univ')
for (jj in 1:L) {
  filename = paste('./2014/', Files[jj], sep = "");
  doc = xmlTreeParse(filename);
  root = xmlRoot(doc);
  node = root[[1]];
  value = xmlfun(node);
  dt[jj,] = value;
}

  