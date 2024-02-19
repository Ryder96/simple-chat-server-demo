variable "project-name" {
    description = "project name"
    default = "simple_chat_server_demo"
}

variable "aws-region" {
    default = "eu-west-1"
}

variable "account-id" {
    default = "891377099355"
}

provider "aws" {
    region = var.aws-region
    profile = "personal"
}


