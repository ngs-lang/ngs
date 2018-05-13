% NA(1) NGS User Manual
% Ilya Sher
% 2018

# NAME

na - NGS AWS. Command line for AWS.

# SYNOPSIS

**na** resource-specification
**na** resource-specification SET new-tags-and-properties
**na** resource-specification DEL
**na** resource-specification COUNT

# DESCRIPTION

**na** is a thin wrapper around AWS library in NGS, which in turn uses declarative primitives approach. This means among other things that **na** is idempotent.

**na** Allows to manipulate AWS resources. The first variant, without any command shows the specified resource(s). "SET" variant is used for updating the specified resource(s). "DEL" deletes the resources. "COUNT" shows the number of specified resources.

# OPTIONS

**\-\-digest N** (default 10). **na** is human oriented. Since it's usually not helpful to have thousands of lines dumped into your screen (not likely to be processed in a meaningful manner by a human), **na** limits the number of items it outputs. **\-\-digest** gives the number of items starting at which the output is shown as a digest of the items and not the items themselves.

# SPECIFYING RESOURCES

## Option 1 - resource id

RESOURCE-ID -- vpc-11111111, subnet-22222222, etc

## Option 2 - resource type + tags + properties

RESOURCE-PREFIX TAG\_NAME=TAG\_VALUE ... PROPERTY\_NAME:PROPERTY\_VALUE ...

Where RESOURCE-PREFIX - i, vpc, sg and others as seen in output of `aws ... describe-...` commands.

## Related resources

**resource-specification** can be repeated on the command line. In this case the specified resource to show or operate is the resource specified on the right. When **na** will look up resources on the right, it will restrict the search to resources mentioned to the left:

	# Show all subnets in VPC which is has tag "env" with value "proxy-lab"
	na vpc env=proxy-lab subnet

# SPECIFYING NEW TAGS AND PROPERTIES

The syntax after "SET" is TAG\_NAME=TAG\_VALUE ... PROPERTY\_NAME:PROPERTY\_VALUE ... (which is the same syntax as for specified resources option 2 just without the RESOURCE-PREFIX)

# EXAMPLES

	# Show all VPCs and Subnets
	na vpc
	na subnet

	# Stop these VMs
	na i role=ocsp-proxy env=dev SET State:stopped

	# Show these VMs
	na i role=ocsp-proxy env=dev

	# Start these VMs
	na i role=ocsp-proxy env=dev State:'*' SET State:running

	# Show all subnets in VPC which is has tag "env" with value "proxy-lab"
	# "Related resources" concept
	na vpc env=proxy-lab subnet

	# Show how many VPCs you have (in current region as seen by AWS CLI)
	na vpc COUNT
