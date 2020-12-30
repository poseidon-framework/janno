# validating a minimal janno file works as expected

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["source_file", "row", "column", "value", "issue"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": []
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": []
        },
        {
          "type": "integer",
          "attributes": {},
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        }
      ]
    }

# validating a normal janno file works as expected

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["source_file", "row", "column", "value", "issue"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": []
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": []
        },
        {
          "type": "integer",
          "attributes": {},
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        }
      ]
    }

# validating a broken janno file works as expected

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["source_file", "row", "column", "value", "issue"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno", "testJannoFiles/borked.janno"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Individual_ID", "Latitude", "Longitude", "Date_BC_AD_Median", "Date_BC_AD_Start", "Date_BC_AD_Stop", "Date_Type", "No_of_Libraries", "Data_Type", "Genotype_Ploidy", "Group_Name", "Genetic_Sex", "Nr_autosomal_SNPs", "Coverage_1240K", "Endogenous", "UDG", "Library_Built", "Damage", "Xcontam", "Xcontam_stderr", "mtContam", "mtContam_stderr"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["n/a", "1000", "-1000", "something", "something", "something", "something", "something", "something", "something", "n/a", "n/a", "something", "something", "something", "something", "something", "something", "something", "something", "something", "something"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["n/a in a mandatory column", "One or multiple values not in range -90 to 90", "One or multiple values not in range -180 to 180", "One or multiple values are not valid integer numbers", "One or multiple values are not valid integer numbers", "One or multiple values are not valid integer numbers", "At least one value not in C14, contextual, modern", "One or multiple values are not valid integer numbers", "At least one value not in Shotgun, 1240K, OtherCapture, ReferenceGenome", "At least one value not in diploid, haploid", "n/a in a mandatory column", "n/a in a mandatory column", "One or multiple values are not valid integer numbers", "One or multiple values are not valid floating point numbers", "One or multiple values are not valid floating point numbers", "At least one value not in minus, half, plus, mixed", "At least one value not in ds, ss, other", "One or multiple values are not valid floating point numbers", "One or multiple values are not valid floating point numbers", "One or multiple values are not valid floating point numbers", "One or multiple values are not valid floating point numbers", "One or multiple values are not valid floating point numbers"]
        }
      ]
    }

