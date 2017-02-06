setwd("/Users/guojun/Google Drive/15Fall/homework/lab10/lab")

## Load the XML package (if necessary, also install it first):
if(!(require(XML))){
	install.packages("XML", dep=T);
	require(XML);
}


## Example 1 : NSF award in 2016

## explore the document tree

doctree = xmlParse('award1.xml');

root = xmlRoot(doctree);
xmlName(root);

rtCh = xmlChildren(root);
length(rtCh);
names(rtCh);

ch = xmlChildren(root[[1]]);
head(ch,3);
length(ch);
names(ch);


## Solution 1

# begin with the list of children nodes of the only child node ("Award") of the root.

ch = xmlChildren(rtCh1);
names(ch);

# use xmlValue() function to get the value of each subchild ch$...

V1 = xmlValue(ch$AwardTitle);
V2 = xmlValue(ch$AwardEffectiveDate)
V3 = xmlValue(ch$AwardExpirationDate)
V4 = xmlValue(ch$AwardAmount);
# look at Investigator node;
ch$Investigator; 
# fetch Investigator's name
V5 = paste(xmlValue(ch$Investigator[[1]]), xmlValue(ch$Investigator[[2]]));
# look at Institution node;
ch$Institution;
V6 = xmlValue(ch$Institution[[1]]);
dt1 = as.data.frame(matrix(NA, ncol=6,nrow=1));
dt1[1,] = c(V1, V2, V3, V4, V5, V6)
print(dt1);

# do the same for all the XML files in the 2016 folder to get the information for all the NSF awards in 2016. Wrap the above in a function which takes in a root node of one XML file, returns the needed information for each file.

myxmlGetInfo = function(root){
	lst_ch = xmlChildren(root[[1]]);
	V1 = xmlValue(lst_ch$AwardTitle);
	V2 = xmlValue(lst_ch$AwardEffectiveDate);
	V3 = xmlValue(lst_ch$AwardExpirationDate);
	V4 = xmlValue(lst_ch$AwardAmount);
	V5 = paste(xmlValue(lst_ch$Investigator[[1]]), xmlValue(lst_ch$Investigator[[2]]));
	V6 = xmlValue(lst_ch$Institution[[1]]);
	# another way for V5 and V6;
	#V5 = paste(xmlValue(xmlChildren(lst_ch$Investigator)$FirstName), xmlValue(xmlChildren(lst_ch$Investigator)$LastName));
	#V6 = xmlValue(xmlChildren(lst_ch$Institution)$Name);
	return(c(V1, V2, V3, V4, V5, V6))
}

# To begin processing XML files in the '2016' folder, read in the .xml files in the folder named '2016/' in the working directory, (/ at the end of its name indicating it's a folder, not a single file). The working directory path can be abbreviated as './' now.

Files = system('ls ./2016/', intern=T);
L = length(Files);
head(Files);

# Now we iterate over all the XML files in the folder and apply our myxmlGetInfo() function to each file' root node;

NSF16AWD = as.data.frame(matrix(NA, ncol=6, nrow=L))
names(dt) = c('Title','Start','End','Amount','PI','University');
for( i in 1:L){
	filename = paste("./2016/",Files[i],sep="");
	doc = xmlTreeParse(filename);
	rt = xmlRoot(doc);
	val = myxmlGetInfo(rt);
	NSF16AWD[i,] = val;
}

head(NSF16AWD);



### Example 2 US congress example people.xml

## Explore the document structure

doc = xmlTreeParse("people.xml");
root = xmlRoot(doc);
ls_people = xmlChildren(root); length(ls_people);

person1 = root[[1]]; 
print(person1);
# one child of the root node, many attributes, use xmlGetAttr() to get the attributes
xmlAttrs(person1);
xmlGetAttr(person1, 'id');

# list all children nodes for person1;
ls_person1 = xmlChildren(person1);
names(ls_person1); # only a 'role' child node


person2 = root[[2]]; ls_person2 = xmlChildren(person2);
names(ls_person2); # 3 children nodes, 
#some people in the file has two more more committee assignments, we will only extract their first committee assignment, which is what you get by:
ls_person2$"committee-assignment";


## Solution 2


# from print(person2) we can see that the first 4 attributes we want are in 'person' nodes, the 5th and 6th attributes are in 'role' nodes, the last 'committee' attribute is in 'committee-assignment' node. 

# we write a function that takes in a person, the needed attributes' names, like 'id', 'lastname', etc. and returns the combined attributes' values for this person; as below:

myGetData = function(x, attrPers, attrRole, attrComm){
	data = character(0);
	l1 = length(attrPers);
	l2 = length(attrRole);
	for(i in 1:l1){
		if (!(is.null(xmlGetAttr(x,attrPers[i])))){
			data = c(data, xmlGetAttr(x,attrPers[i]))
		}
		else data = c(data, NA)
	}
	
	ch = xmlChildren(x)$role;
	if(!is.null(ch)){
		for(j in 1:l2){
			if(!(is.null(xmlGetAttr(ch, attrRole[j])))){
				data = c(data, xmlGetAttr(ch, attrRole[j]))
			}
			else data = c(data, NA)
		}
	}
	else data = c(data, NA,NA,NA)
	
	comm = xmlChildren(x)$'committee-assignment';
	if(!is.null(comm)){
		# length(attrComm) is 1, no loop is needed;
		if(!(is.null(xmlGetAttr(comm, comm.attr)))){
			data = c(data, xmlGetAttr(comm, comm.attr))
		}
		else data = c(data, NA)
	}
	else data = c(data, NA)
	
	return(data)

}


# prepare the storage for the output attributes values as a dataframe;
name_var = c('id','lastname','firstname','gender','party','state','committee');
congress_dt = as.data.frame(matrix(NA, ncol=7,nrow=1));
names(congress_dt) = name_var;

# the attributes' names as input to the function myGetData()
pers.attr = name_var[1:4];
role.attr = name_var[5:6];
comm.attr = name_var[7];

# apply our myGetData() function to each person in the xml file, which is each child node of the root node.
doc = xmlTreeParse("people.xml");
root = xmlRoot(doc);
list.person = xmlChildren(root);
for(i in 1:length(list.person)){
	person = list.person[[i]]; # same as root[[i]]
	dt = myGetData(person, pers.attr, role.attr, comm.attr)
	congress_dt[i,] = dt;
}

head(congress_dt);