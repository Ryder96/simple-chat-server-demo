resource "aws_elb" "elastic_balancer" {
  name               = "elastic-balancer"
  availability_zones = ["eu-west-1a", "eu-west-1b", "eu-west-1c"]

  listener {
    instance_port     = 5678
    instance_protocol = "TCP"
    lb_port           = 5678
    lb_protocol       = "TCP"
  }


  health_check {
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 3
    target              = "HTTP:8080/"
    interval            = 30
  }


  instances                   = [aws_instance.erlang_instance.id]
  cross_zone_load_balancing   = true
  idle_timeout                = 400
  connection_draining         = true
  connection_draining_timeout = 400

  tags = {
    Name = "erlang balancer"
  }
}
