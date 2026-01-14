"""S3 utility functions shared between modules."""

import re
from datetime import datetime, timezone
from pathlib import Path


class S3UrlInfo:
    """Data class to hold parsed S3 URL information."""

    def __init__(self, hostname: str = "", bucket: str = "", path: str = "") -> None:
        self.hostname = hostname
        self.bucket = bucket
        self.path = path

    def to_url(self) -> str:
        """Construct the full S3 URL from hostname, bucket, and path."""
        # Ensure hostname has proper protocol
        if not self.hostname.startswith(("http://", "https://")):
            hostname = f"https://{self.hostname}"
        else:
            hostname = self.hostname

        # Remove leading slash from path if present
        path = self.path.lstrip("/")

        # Construct full URL
        return f"{hostname}/{self.bucket}/{path}"

    def to_local(self) -> Path:
        """Construct local path from S3 path by adding 'cases/' prefix and '/case' suffix."""
        path = self.path
        if not path.endswith("/doc"):
            # matches: cases/e.../f.../c...[/anything/...]
            case_pattern = re.compile(r"^cases/[eE][^/]*/[fF][^/]*/[cC][^/]*(?:/[^/]+)*$")
            if case_pattern.match(path):
                path = path + "/input"
            elif path.startswith("references/"):
                path = path.replace("references/", "")
                if path.startswith("lnx64"):
                    path = path.replace("lnx64/", "cases/")
                    path = path + "/reference_lnx64"
                elif path.startswith("win64"):
                    path = path.replace("win64/", "cases/")
                    path = path + "/reference_win64"

        return Path(f"data/{path}")


def timestamp_str_2_datetime(timestamp: str) -> datetime | None:
    """
    Parse timestamp string for rewind functionality.

    Args:
        timestamp: Timestamp string to parse

    Returns
    -------
        Parsed datetime object or None if invalid
    """
    if not timestamp or timestamp == "NO VERSION":
        return None

    try:
        # Try different timestamp formats
        formats = [
            "%Y-%m-%dT%H:%M:%S.%f",  # With microseconds: 'e.g. 2025-09-11T13:20:21.667000'
            "%Y-%m-%dT%H:%M:%S",  # Without microseconds: 'e.g. 2025-09-11T13:20:21'
            "%Y-%m-%dT%H:%M",  # Without seconds: 'e.g. 2025-11-19T10:31'
        ]

        for fmt in formats:
            try:
                return datetime.strptime(timestamp, fmt).replace(tzinfo=timezone.utc)
            except ValueError:
                continue

        return None
    except Exception:
        return None
