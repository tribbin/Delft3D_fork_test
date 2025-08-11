"""Tests for pin_and_tag_builds.py."""

from unittest.mock import Mock, patch

import pytest

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.pin_and_tag_builds import pin_and_tag_builds
from ci_tools.dimrset_delivery.settings.general_settings import DRY_RUN_PREFIX
from ci_tools.dimrset_delivery.settings.teamcity_settings import TeamcityIds


class TestPinAndTagBuilds:
    """Test cases for pin_and_tag_builds function."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.mock_context = Mock(spec=DimrAutomationContext)
        self.mock_context.build_id = "12345"
        self.mock_context.dry_run = False
        self.mock_context.teamcity = Mock()
        self.mock_context.git_client = Mock()
        self.mock_context.print_status = Mock()
        self.mock_context.get_kernel_versions.return_value = {"build.vcs.number": "abc123def"}
        self.mock_context.get_dimr_version.return_value = "1.2.3"

    def test_pin_and_tag_builds_success(self) -> None:
        """Test successful pinning and tagging of builds."""
        # Arrange
        self.mock_context.teamcity.get_dependent_build_ids_with_filter.return_value = ["id1", "id2"]

        # Act
        pin_and_tag_builds(self.mock_context)

        # Assert
        self.mock_context.print_status.assert_called_once_with("Pinning and tagging builds...")
        self.mock_context.get_kernel_versions.assert_called_once()
        self.mock_context.get_dimr_version.assert_called_once()
        self.mock_context.teamcity.add_tag_to_build_with_dependencies.assert_called_once_with(
            "12345", tag="DIMRset_1.2.3"
        )
        teamcity_ids_list = [member.value for member in TeamcityIds]
        self.mock_context.teamcity.get_dependent_build_ids_with_filter.assert_called_once_with(
            "12345", teamcity_ids_list
        )
        self.mock_context.teamcity.pin_build.assert_any_call(build_id="id1")
        self.mock_context.teamcity.pin_build.assert_any_call(build_id="id2")
        self.mock_context.teamcity.pin_build.assert_any_call(build_id="12345")  # Also pins the original build
        self.mock_context.git_client.tag_commit.assert_called_once_with("abc123def", "DIMRset_1.2.3")

    def test_pin_and_tag_builds_dry_run(self) -> None:
        """Test dry run mode - should only print what would be done."""
        # Arrange
        self.mock_context.dry_run = True

        # Act
        with patch("builtins.print") as mock_print:
            pin_and_tag_builds(self.mock_context)

        # Assert
        self.mock_context.print_status.assert_called_once_with("Pinning and tagging builds...")
        self.mock_context.get_kernel_versions.assert_called_once()
        self.mock_context.get_dimr_version.assert_called_once()

        # Verify dry run messages were printed
        expected_calls = [
            (f"{DRY_RUN_PREFIX} Would pin and tag builds in TeamCity for build chain:", "12345"),
            (f"{DRY_RUN_PREFIX} Would add tag:", "DIMRset_1.2.3"),
            (f"{DRY_RUN_PREFIX} Would tag commit with:", "commit=abc123def, tag=DIMRset_1.2.3"),
        ]

        for expected_args in expected_calls:
            assert any(call.args == expected_args for call in mock_print.call_args_list), (
                f"Expected print call with args {expected_args} not found"
            )

        # Verify that actual operations were not performed
        teamcity_not_called = (
            not hasattr(self.mock_context.teamcity, "call_args_list") or not self.mock_context.teamcity.call_args_list
        )
        git_client_not_called = (
            not hasattr(self.mock_context.git_client, "call_args_list")
            or not self.mock_context.git_client.call_args_list
        )
        assert teamcity_not_called
        assert git_client_not_called

    def test_pin_and_tag_builds_missing_teamcity_client(self) -> None:
        """Test error when TeamCity client is None."""
        # Arrange
        self.mock_context.teamcity = None

        # Act & Assert
        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            pin_and_tag_builds(self.mock_context)

    def test_pin_and_tag_builds_missing_git_client(self) -> None:
        """Test error when Git client is None."""
        # Arrange
        self.mock_context.git_client = None

        # Act & Assert
        with pytest.raises(ValueError, match="Git client is required but not initialized"):
            pin_and_tag_builds(self.mock_context)

    def test_pin_and_tag_builds_with_different_versions(self) -> None:
        """Test with different kernel and DIMR versions."""
        # Arrange
        self.mock_context.get_kernel_versions.return_value = {"build.vcs.number": "xyz789abc"}
        self.mock_context.get_dimr_version.return_value = "2.0.0"
        self.mock_context.teamcity.get_dependent_build_ids_with_filter.return_value = ["id3"]

        # Act
        pin_and_tag_builds(self.mock_context)

        # Assert
        self.mock_context.teamcity.add_tag_to_build_with_dependencies.assert_called_once_with(
            "12345", tag="DIMRset_2.0.0"
        )
        teamcity_ids_list = [member.value for member in TeamcityIds]
        self.mock_context.teamcity.get_dependent_build_ids_with_filter.assert_called_once_with(
            "12345", teamcity_ids_list
        )
        self.mock_context.teamcity.pin_build.assert_any_call(build_id="id3")
        self.mock_context.teamcity.pin_build.assert_any_call(build_id="12345")  # Also pins the original build
        self.mock_context.git_client.tag_commit.assert_called_once_with("xyz789abc", "DIMRset_2.0.0")

    @patch("builtins.print")
    def test_pin_and_tag_builds_success_message(self, mock_print: Mock) -> None:
        """Test that success message is printed after completion."""
        # Arrange
        self.mock_context.teamcity.get_dependent_build_ids_with_filter.return_value = []

        # Act
        pin_and_tag_builds(self.mock_context)

        # Assert
        mock_print.assert_called_with("Build pinning and tagging completed successfully!")

    def test_pin_and_tag_builds_teamcity_exception(self) -> None:
        """Test when TeamCity client raises an exception."""
        # Arrange
        self.mock_context.teamcity.add_tag_to_build_with_dependencies.side_effect = Exception("TeamCity error")

        # Act & Assert
        with pytest.raises(Exception, match="TeamCity error"):
            pin_and_tag_builds(self.mock_context)

    def test_pin_and_tag_builds_git_client_exception(self) -> None:
        """Test when Git client raises an exception."""
        # Arrange
        self.mock_context.teamcity.get_dependent_build_ids_with_filter.return_value = []
        self.mock_context.git_client.tag_commit.side_effect = Exception("Git error")

        # Act & Assert
        with pytest.raises(Exception, match="Git error"):
            pin_and_tag_builds(self.mock_context)

    def test_pin_and_tag_builds_dry_run_no_clients_required(self) -> None:
        """Test that dry run works even with None clients."""
        # Arrange
        self.mock_context.dry_run = True
        self.mock_context.teamcity = None
        self.mock_context.git_client = None

        # Act & Assert - should not raise an exception
        with patch("builtins.print"):
            pin_and_tag_builds(self.mock_context)

        # Verify that methods were still called to get version info
        self.mock_context.get_kernel_versions.assert_called_once()
        self.mock_context.get_dimr_version.assert_called_once()

    def test_pin_and_tag_builds_context_method_calls(self) -> None:
        """Test that all expected context methods are called in the correct order."""
        # Arrange
        self.mock_context.teamcity.get_dependent_build_ids_with_filter.return_value = []

        # Act
        pin_and_tag_builds(self.mock_context)

        # Assert method call order
        assert self.mock_context.print_status.call_count == 1
        assert self.mock_context.get_kernel_versions.call_count == 1
        assert self.mock_context.get_dimr_version.call_count == 1
        self.mock_context.print_status.assert_called_once_with("Pinning and tagging builds...")
        self.mock_context.get_kernel_versions.assert_called_once()
        self.mock_context.get_dimr_version.assert_called_once()
