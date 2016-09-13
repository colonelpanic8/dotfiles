#!/usr/bin/perl
# Quickly set ForceFullCompositionPipeline with nvidia-settings for all currently connected screens, useful for gamers seeing screen tear issues
use strict;
use warnings;

# Grab xrandr output for parsing
my $xrandrOutput = `xrandr`;

# Filter all currently selected modes
my @modes = $xrandrOutput =~ /.*\sconnected.*/g;
die "Output didn't match expected format\n" if ( scalar @modes lt 1 );
my @newModes = ();
foreach my $mode ( @modes )
{
	# Filter the current modes and add to list in right format
	my @details = $mode =~ /(.*)\sconnected.*?(\d[^\s]+).*/g;
	die "Output didn't match expected format\n" if ( scalar @details lt 2 );
	push( @newModes, join( ":", @details ) );
}

# Construct the new mode command
my $newModeCommand = "nvidia-settings --assign CurrentMetaMode='";
$newModeCommand .= "$_ { ForceFullCompositionPipeline = On }, " foreach ( @newModes );
$newModeCommand .= "'";

# Print and set our new mode
print "running \"$newModeCommand\"\n";
system( $newModeCommand );
