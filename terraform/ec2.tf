resource "aws_key_pair" "ssh_key" {
  key_name   = "ssh_key"
  public_key = file("~/.ssh/id_rsa.pub")
}

resource "aws_instance" "erlang-instance" {
  ami           = "ami-08031206a0ff5a6ac"
  instance_type = "t2.micro"
  key_name      = aws_key_pair.ssh_key.key_name
  tags = {
    Name = "erlang chat server demo"
  }
}
