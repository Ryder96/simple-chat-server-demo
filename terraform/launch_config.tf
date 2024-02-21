resource "aws_launch_configuration" "my_configuration" {
  name          = "erlang-config"
  image_id      = aws_instance.erlang_instance.ami
  instance_type = "t2.micro"

  security_groups = ["${aws_security_group.instace_security_group.id}"]
  key_name   = "ssh_key"


  lifecycle {
    create_before_destroy = true
  }
}
