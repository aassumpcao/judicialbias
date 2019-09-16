from stdnum.exceptions import *

def _to_base10(number):
    """Prepare the number to its base10 representation."""
    try:
        return ''.join(
            str(int(x, 36)) for x in number)
    except Exception:
        raise InvalidFormat()

def checksum(number):
    """Calculate the checksum. A valid number should have a checksum of 1."""
    return int(_to_base10(number)) % 97

def calc_check_digits(number):
    """Calculate the extra digits that should be appended to the number to
    make it a valid number."""
    return '%02d' % ((98 - 100 * checksum(number)) % 97)

def validate(number):
    """Check whether the check digit is valid."""
    try:
        valid = checksum(number) == 1
    except Exception:
        raise InvalidFormat()
    if not valid:
        raise InvalidChecksum()
    return number

def is_valid(number):
    """Check whether the check digit is valid."""
    try:
        return bool(validate(number))
    except ValidationError:
        return False
