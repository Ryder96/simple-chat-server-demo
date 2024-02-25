resource "aws_dynamodb_table" "rooms" {
  name           = "Rooms"
  billing_mode   = "PROVISIONED"
  hash_key       = "Name"
  range_key      = "Owner"
  read_capacity  = 5
  write_capacity = 5

  attribute {
    name = "Name"
    type = "S"
  }

  attribute {
    name = "Owner"
    type = "S"
  }
}