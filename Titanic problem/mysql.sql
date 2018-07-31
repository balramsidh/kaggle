CREATE TABLE train (
	`PassengerId` DECIMAL NOT NULL, 
	`Survived` BOOL NOT NULL, 
	`Pclass` DECIMAL NOT NULL, 
	`Name` VARCHAR(82) NOT NULL, 
	`Sex` VARCHAR(6) NOT NULL, 
	`Age` DECIMAL, 
	`SibSp` DECIMAL NOT NULL, 
	`Parch` DECIMAL NOT NULL, 
	`Ticket` VARCHAR(18) NOT NULL, 
	`Fare` DECIMAL NOT NULL, 
	`Cabin` VARCHAR(15), 
	`Embarked` VARCHAR(1), 
	CHECK (`Survived` IN (0, 1))
);
