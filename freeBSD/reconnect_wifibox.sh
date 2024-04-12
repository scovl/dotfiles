#!/bin/sh

# Use environment variables for configuration, allowing customization without editing the script
NETWORK_INTERFACE="${NETWORK_INTERFACE:-wifibox0}"
TEST_IP="${TEST_IP:-8.8.8.8}"
WPA_SUPPLICANT_CONF="/etc/wpa_supplicant.conf"

# Function to check connectivity
check_connectivity() {
    ping -c 1 "$TEST_IP" > /dev/null 2>&1
    return $?
}

# Function to reconnect
reconnect() {
  echo "Connection lost detected. Trying to reconnect..."
    # Unassociate the Wi-Fi interface
    ifconfig "$NETWORK_INTERFACE" -nwid
    # Request a new DHCP connection
    dhclient "$NETWORK_INTERFACE"
}

# Function to connect to a WPA/WPA2 protected Wi-Fi network
connect_wpa() {
    echo "Connecting to a WPA/WPA2 protected Wi-Fi network..."
    wpa_supplicant -i "$NETWORK_INTERFACE" -c "$WPA_SUPPLICANT_CONF"
    # After connecting, request a new DHCP connection
    dhclient "$NETWORK_INTERFACE"
}

# Function to check if dhclient is running and kill it
check_dhclient() {
    if pgrep -x "dhclient" > /dev/null
    then
        echo "dhclient is running. Killing it..."
        pkill dhclient
    else
        echo "dhclient is not running."
    fi
}

# Check if dhclient is running and kill it if necessary
check_dhclient

# Check connectivity and reconnect if necessary
if ! check_connectivity; then
    reconnect
else
    echo "Active connection. No action needed."
fi

# If needed, connect to a WPA/WPA2 protected Wi-Fi network
# connect_wpa
