# validating broken_full.janno works (list of issues)

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
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]
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
          "value": ["testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno", "testJannoFiles/borked_full.janno"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 13, 12, 2, 3, 8, 6, 5, 7, 9, 10, 17, 18, 11, 16, 14, 15, 19]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Poseidon_ID", "Genetic_Sex", "Group_Name", "Latitude", "Longitude", "Date_Type", "Date_BC_AD_Start", "Date_BC_AD_Median", "Date_BC_AD_Stop", "Nr_Libraries", "Capture_Type", "UDG", "Library_Built", "Genotype_Ploidy", "Endogenous", "Nr_SNPs", "Coverage_on_Target_SNPs", "Damage"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["n/a", "n/a", "n/a", "1000", "-1000", "something", "something", "something", "something", "something", "something", "something", "something", "something", "something", "something", "something", "something"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["n/a in a mandatory column", "n/a in a mandatory column", "n/a in a mandatory column", "One or multiple values not in range -90 to 90", "One or multiple values not in range -180 to 180", "At least one value not in C14, contextual, modern", "One or multiple values are not valid integer numbers", "One or multiple values are not valid integer numbers", "One or multiple values are not valid integer numbers", "One or multiple values are not valid integer numbers", "At least one value not in Shotgun, 1240K, ArborComplete, ArborPrimePlus, ArborAncestralPlus, TwistAncientDNA, OtherCapture, ReferenceGenome", "At least one value not in minus, half, plus, mixed", "At least one value not in ds, ss, other", "At least one value not in diploid, haploid", "One or multiple values are not valid floating point numbers", "One or multiple values are not valid integer numbers", "One or multiple values are not valid floating point numbers", "One or multiple values are not valid floating point numbers"]
        }
      ]
    }

# validating broken_partial.janno works (one issue)

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
          "value": [1]
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
          "value": ["testJannoFiles/borked_partial.janno"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Latitude"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["1000"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["One or multiple values not in range -90 to 90"]
        }
      ]
    }

# validating minimal_full.janno works (no issues)

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

# validating minimal_partial.janno works (no issues)

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

# validating minimal_partial_extra_columns.janno works (no issues)

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
          "value": [1, 2]
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
          "value": ["testJannoFiles/minimal_partial_extra_columns.janno", "testJannoFiles/minimal_partial_extra_columns.janno"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [null, null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Testcolumn1", "Tesctolumn2"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [null, null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["This column is not defined in the Poseidon schema implemented in this package. It will be read as a character column. Maybe you mistyped the column name  Relation_To?", "This column is not defined in the Poseidon schema implemented in this package. It will be read as a character column. Maybe you mistyped the column name  Genetic_Sex?"]
        }
      ]
    }

# validating normal_full.janno works (no issues)

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

# validating normal_partial.janno works (no issues)

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

