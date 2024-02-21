resource "aws_autoscaling_group" "auto_scaler" {

  launch_configuration = "${aws_launch_configuration.my_configuration.id}"
  name = "test-autoscaling-group"
  availability_zones = ["eu-west-1a","eu-west-1b","eu-west-1c"]
  min_size             = 2
  max_size             = 4
  desired_capacity     = 2
  load_balancers = ["${aws_elb.elastic_balancer.name}"]
  health_check_type = "ELB"

}
