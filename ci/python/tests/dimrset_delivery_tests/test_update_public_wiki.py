"""Tests for update_public_wiki.py."""

from datetime import datetime, timezone
from unittest.mock import Mock, mock_open, patch

import pytest

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.atlassian import Atlassian
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.atlassian_settings import (
    DIMR_MAJOR_PAGE_PREFIX,
    DIMR_MINOR_PAGE_PREFIX,
    DIMR_PATCH_PAGE_PREFIX,
    DIMR_ROOT_PAGE_ID,
    DIMR_SPACE_ID,
    DIMR_SUBPAGE_PREFIX,
    DIMR_SUBPAGE_SUFFIX,
    RELATIVE_PATH_TO_WIKI_TEMPLATE,
)
from ci_tools.dimrset_delivery.settings.general_settings import DRY_RUN_PREFIX
from ci_tools.dimrset_delivery.settings.teamcity_settings import (
    PATH_TO_LINUX_VERSION_ARTIFACT,
    PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT,
    PATH_TO_WINDOWS_VERSION_ARTIFACT,
    TeamcityIds,
)
from ci_tools.dimrset_delivery.update_public_wiki import PublicWikiHelper, update_public_wiki


class TestPublicWikiHelper:
    """Test cases for PublicWikiHelper class."""

    @pytest.fixture
    def mock_atlassian(self) -> Mock:
        """Create a mock Atlassian client."""
        return Mock(spec=Atlassian)

    @pytest.fixture
    def mock_teamcity(self) -> Mock:
        """Create a mock TeamCity client."""
        return Mock(spec=TeamCity)

    @pytest.fixture
    def dimr_version(self) -> str:
        """Provide a test DIMR version."""
        return "2.13.03"

    @pytest.fixture
    def public_wiki_helper(self, mock_atlassian: Mock, mock_teamcity: Mock, dimr_version: str) -> PublicWikiHelper:
        """Create a PublicWikiHelper instance for testing."""
        return PublicWikiHelper(mock_atlassian, mock_teamcity, dimr_version)

    def test_init_sets_attributes_correctly(self, mock_atlassian: Mock, mock_teamcity: Mock, dimr_version: str) -> None:
        """Test that __init__ correctly sets all attributes."""
        # Arrange & Act
        helper = PublicWikiHelper(mock_atlassian, mock_teamcity, dimr_version)

        # Assert
        assert helper._PublicWikiHelper__atlassian == mock_atlassian
        assert helper._PublicWikiHelper__teamcity == mock_teamcity
        assert helper._PublicWikiHelper__dimr_version == dimr_version
        assert helper._PublicWikiHelper__major_version == "2"
        assert helper._PublicWikiHelper__minor_version == "13"
        assert helper._PublicWikiHelper__patch_version == "03"

    @patch.object(PublicWikiHelper, "_PublicWikiHelper__update_main_page")
    @patch.object(PublicWikiHelper, "_PublicWikiHelper__update_sub_page")
    def test_update_public_wiki_calls_both_update_methods(
        self, mock_update_sub_page: Mock, mock_update_main_page: Mock, public_wiki_helper: PublicWikiHelper
    ) -> None:
        """Test that update_public_wiki calls both main and sub page update methods."""
        # Arrange
        build_id_chain = "123456"
        main_page_id = "main_page_123"
        mock_update_main_page.return_value = main_page_id

        # Act
        public_wiki_helper.update_public_wiki(build_id_chain)

        # Assert
        mock_update_main_page.assert_called_once_with(build_id_chain)
        mock_update_sub_page.assert_called_once_with(parent_page_id=main_page_id)

    @patch.object(PublicWikiHelper, "_PublicWikiHelper__prepare_content_for_main_page")
    @patch.object(PublicWikiHelper, "_PublicWikiHelper__get_main_page_id_for_page_to_update")
    @patch.object(PublicWikiHelper, "_PublicWikiHelper__update_content_of_page")
    def test_update_main_page_returns_correct_page_id(
        self,
        mock_update_content: Mock,
        mock_get_page_id: Mock,
        mock_prepare_content: Mock,
        public_wiki_helper: PublicWikiHelper,
    ) -> None:
        """Test that __update_main_page returns the correct page ID."""
        # Arrange
        build_id_chain = "123456"
        content = "<html>Test content</html>"
        page_id = "page_123"
        expected_title = f"{DIMR_PATCH_PAGE_PREFIX} 2.13.03"

        mock_prepare_content.return_value = content
        mock_get_page_id.return_value = page_id

        # Act
        result = public_wiki_helper._PublicWikiHelper__update_main_page(build_id_chain)

        # Assert
        assert result == page_id
        mock_prepare_content.assert_called_once_with(build_id_chain)
        mock_get_page_id.assert_called_once()
        mock_update_content.assert_called_once_with(page_id, expected_title, content)

    @patch.object(PublicWikiHelper, "_PublicWikiHelper__get_version_artifacts")
    @patch.object(PublicWikiHelper, "_PublicWikiHelper__create_content_from_template")
    def test_prepare_content_for_main_page_success(
        self,
        mock_create_content: Mock,
        mock_get_artifacts: Mock,
        public_wiki_helper: PublicWikiHelper,
    ) -> None:
        """Test successful preparation of main page content."""
        # Arrange
        build_id_chain = "123456"
        windows_artifact = "Windows version content"
        linux_artifact = "Linux version content"
        expected_content = "<html>Generated content</html>"

        mock_get_artifacts.return_value = (windows_artifact, linux_artifact)
        mock_create_content.return_value = expected_content

        # Act
        result = public_wiki_helper._PublicWikiHelper__prepare_content_for_main_page(build_id_chain)

        # Assert
        assert result == expected_content
        mock_get_artifacts.assert_called_once_with(build_id_chain)
        mock_create_content.assert_called_once_with(
            windows_version_artifact=windows_artifact, linux_version_artifact=linux_artifact
        )

    @patch.object(PublicWikiHelper, "_PublicWikiHelper__get_version_artifacts")
    def test_prepare_content_for_main_page_raises_assertion_error_when_windows_artifact_is_none(
        self, mock_get_artifacts: Mock, public_wiki_helper: PublicWikiHelper
    ) -> None:
        """Test that AssertionError is raised when Windows artifact is None."""
        # Arrange
        build_id_chain = "123456"
        mock_get_artifacts.return_value = (None, "Linux content")

        # Act & Assert
        with pytest.raises(AssertionError, match="Could not retrieve the Windows version.txt artifact."):
            public_wiki_helper._PublicWikiHelper__prepare_content_for_main_page(build_id_chain)

    @patch.object(PublicWikiHelper, "_PublicWikiHelper__get_version_artifacts")
    def test_prepare_content_for_main_page_raises_assertion_error_when_linux_artifact_is_none(
        self, mock_get_artifacts: Mock, public_wiki_helper: PublicWikiHelper
    ) -> None:
        """Test that AssertionError is raised when Linux artifact is None."""
        # Arrange
        build_id_chain = "123456"
        mock_get_artifacts.return_value = ("Windows content", None)

        # Act & Assert
        with pytest.raises(AssertionError, match="Could not retrieve the Linux version.txt artifact."):
            public_wiki_helper._PublicWikiHelper__prepare_content_for_main_page(build_id_chain)

    def test_get_version_artifacts_success(self, public_wiki_helper: PublicWikiHelper, mock_teamcity: Mock) -> None:
        """Test successful retrieval of version artifacts."""
        # Arrange
        build_id_chain = "123456"
        windows_collect_id = 789
        linux_collect_id = 101112
        windows_artifact_bytes = b"Windows version 2.13.03"
        linux_artifact_bytes = b"Linux version 2.13.03"

        mock_teamcity.get_dependent_build_id.side_effect = [windows_collect_id, linux_collect_id]
        mock_teamcity.get_build_artifact.side_effect = [windows_artifact_bytes, linux_artifact_bytes]

        # Act
        windows_result, linux_result = public_wiki_helper._PublicWikiHelper__get_version_artifacts(build_id_chain)

        # Assert
        assert windows_result == "Windows version 2.13.03"
        assert linux_result == "Linux version 2.13.03"
        assert mock_teamcity.get_dependent_build_id.call_count == 2
        assert mock_teamcity.get_build_artifact.call_count == 2

        # Verify specific calls
        mock_teamcity.get_dependent_build_id.assert_any_call(
            build_id_chain, TeamcityIds.DELFT3D_WINDOWS_COLLECT_BUILD_TYPE_ID.value
        )
        mock_teamcity.get_dependent_build_id.assert_any_call(
            build_id_chain, TeamcityIds.DELFT3D_LINUX_COLLECT_BUILD_TYPE_ID.value
        )
        mock_teamcity.get_build_artifact.assert_any_call(
            build_id=str(windows_collect_id), path_to_artifact=PATH_TO_WINDOWS_VERSION_ARTIFACT
        )
        mock_teamcity.get_build_artifact.assert_any_call(
            build_id=str(linux_collect_id), path_to_artifact=PATH_TO_LINUX_VERSION_ARTIFACT
        )

    def test_get_version_artifacts_raises_value_error_when_artifacts_are_none(
        self, public_wiki_helper: PublicWikiHelper, mock_teamcity: Mock
    ) -> None:
        """Test that ValueError is raised when artifacts cannot be retrieved."""
        # Arrange
        build_id_chain = "123456"
        mock_teamcity.get_dependent_build_id.return_value = 123
        mock_teamcity.get_build_artifact.return_value = None

        # Act & Assert
        with pytest.raises(ValueError, match="Could not retrieve version artifacts"):
            public_wiki_helper._PublicWikiHelper__get_version_artifacts(build_id_chain)

    @patch("ci_tools.dimrset_delivery.update_public_wiki.datetime")
    @patch(
        "builtins.open",
        new_callable=mock_open,
        read_data=(
            "Template @@@DATE@@@ @@@WINDOWS_VERSION_ARTIFACT@@@ @@@LINUX_VERSION_ARTIFACT@@@ @@@DIMR_RELEASE_VERSION@@@"
        ),
    )
    @patch("os.path.dirname")
    @patch("os.path.join")
    def test_create_content_from_template_success(
        self,
        mock_join: Mock,
        mock_dirname: Mock,
        mock_file: Mock,
        mock_datetime: Mock,
        public_wiki_helper: PublicWikiHelper,
    ) -> None:
        """Test successful content creation from template."""
        # Arrange
        windows_artifact = "Windows 2.13.03"
        linux_artifact = "Linux 2.13.03"
        current_dir = "/test/dir"
        template_path = "/test/dir/template.html"
        test_date = datetime(2023, 8, 15, tzinfo=timezone.utc)

        mock_dirname.return_value = current_dir
        mock_join.return_value = template_path
        mock_datetime.now.return_value = test_date

        # Act
        result = public_wiki_helper._PublicWikiHelper__create_content_from_template(windows_artifact, linux_artifact)

        # Assert
        expected_content = "Template 15-08-2023 <pre>Windows 2.13.03</pre> <pre>Linux 2.13.03</pre> 2.13.03"
        assert result == expected_content
        mock_dirname.assert_called_once()
        mock_join.assert_called_once_with(current_dir, RELATIVE_PATH_TO_WIKI_TEMPLATE)
        mock_file.assert_called_once_with(template_path, "r")

    @patch.object(PublicWikiHelper, "_PublicWikiHelper__get_public_wiki_page_id")
    def test_get_main_page_id_for_page_to_update_success(
        self, mock_get_page_id: Mock, public_wiki_helper: PublicWikiHelper
    ) -> None:
        """Test successful retrieval of main page ID for update."""
        # Arrange
        major_page_id = "major_123"
        minor_page_id = "minor_456"
        patch_page_id = "patch_789"

        mock_get_page_id.side_effect = [major_page_id, minor_page_id, patch_page_id]

        # Act
        result = public_wiki_helper._PublicWikiHelper__get_main_page_id_for_page_to_update()

        # Assert
        assert result == patch_page_id
        assert mock_get_page_id.call_count == 3

        # Verify the calls
        mock_get_page_id.assert_any_call(
            parent_page_id=DIMR_ROOT_PAGE_ID, dimr_version="2", prefix=DIMR_MAJOR_PAGE_PREFIX
        )
        mock_get_page_id.assert_any_call(
            parent_page_id=major_page_id, dimr_version="2.13", prefix=DIMR_MINOR_PAGE_PREFIX
        )
        mock_get_page_id.assert_any_call(
            parent_page_id=minor_page_id, dimr_version="2.13.03", prefix=DIMR_PATCH_PAGE_PREFIX
        )

    def test_get_public_wiki_page_id_existing_page(
        self, public_wiki_helper: PublicWikiHelper, mock_atlassian: Mock
    ) -> None:
        """Test retrieval of existing page ID."""
        # Arrange
        parent_page_id = "parent_123"
        dimr_version = "2.13"
        prefix = "DIMRset"
        suffix = ""
        expected_page_id = "existing_page_456"

        mock_parent_page = {
            "results": [
                {"title": "Other Page", "id": "other_123"},
                {"title": f"{prefix} {dimr_version}{suffix}", "id": expected_page_id},
                {"title": "Another Page", "id": "another_789"},
            ]
        }
        mock_atlassian.get_page_info_for_parent_page.return_value = mock_parent_page

        # Act
        result = public_wiki_helper._PublicWikiHelper__get_public_wiki_page_id(
            parent_page_id, dimr_version, prefix, suffix
        )

        # Assert
        assert result == expected_page_id
        mock_atlassian.get_page_info_for_parent_page.assert_called_once_with(parent_page_id=parent_page_id)
        mock_atlassian.create_public_wiki_page.assert_not_called()

    def test_get_public_wiki_page_id_creates_new_page(
        self, public_wiki_helper: PublicWikiHelper, mock_atlassian: Mock
    ) -> None:
        """Test creation of new page when it doesn't exist."""
        # Arrange
        parent_page_id = "parent_123"
        dimr_version = "2.13"
        prefix = "DIMRset"
        suffix = ""
        created_page_id = "new_page_456"
        expected_title = f"{prefix} {dimr_version}{suffix}"

        mock_parent_page = {
            "results": [
                {"title": "Other Page", "id": "other_123"},
                {"title": "Another Page", "id": "another_789"},
            ]
        }
        mock_atlassian.get_page_info_for_parent_page.return_value = mock_parent_page
        mock_atlassian.create_public_wiki_page.return_value = created_page_id

        # Act
        result = public_wiki_helper._PublicWikiHelper__get_public_wiki_page_id(
            parent_page_id, dimr_version, prefix, suffix
        )

        # Assert
        assert result == created_page_id
        mock_atlassian.get_page_info_for_parent_page.assert_called_once_with(parent_page_id=parent_page_id)
        mock_atlassian.create_public_wiki_page.assert_called_once_with(
            page_title=expected_title, space_id=DIMR_SPACE_ID, ancestor_id=parent_page_id
        )

    def test_get_public_wiki_page_id_raises_value_error_when_parent_page_not_found(
        self, public_wiki_helper: PublicWikiHelper, mock_atlassian: Mock
    ) -> None:
        """Test that ValueError is raised when parent page cannot be retrieved."""
        # Arrange
        parent_page_id = "parent_123"
        dimr_version = "2.13"
        prefix = "DIMRset"
        suffix = ""

        mock_atlassian.get_page_info_for_parent_page.return_value = None

        # Act & Assert
        with pytest.raises(ValueError, match=f"Could not retrieve parent page info for page ID: {parent_page_id}"):
            public_wiki_helper._PublicWikiHelper__get_public_wiki_page_id(parent_page_id, dimr_version, prefix, suffix)

    def test_get_public_wiki_page_id_raises_value_error_when_page_creation_fails(
        self, public_wiki_helper: PublicWikiHelper, mock_atlassian: Mock
    ) -> None:
        """Test that ValueError is raised when page creation fails."""
        # Arrange
        parent_page_id = "parent_123"
        dimr_version = "2.13"
        prefix = "DIMRset"
        suffix = ""
        expected_title = f"{prefix} {dimr_version}{suffix}"

        mock_parent_page = {"results": []}
        mock_atlassian.get_page_info_for_parent_page.return_value = mock_parent_page
        mock_atlassian.create_public_wiki_page.return_value = None

        # Act & Assert
        with pytest.raises(ValueError, match=f"Failed to create page: {expected_title}"):
            public_wiki_helper._PublicWikiHelper__get_public_wiki_page_id(parent_page_id, dimr_version, prefix, suffix)

    def test_get_public_wiki_page_id_raises_assertion_error_when_page_id_is_empty(
        self, public_wiki_helper: PublicWikiHelper, mock_atlassian: Mock
    ) -> None:
        """Test that AssertionError is raised when page ID is empty or None."""
        # Arrange
        parent_page_id = "parent_123"
        dimr_version = "2.13"
        prefix = "DIMRset"
        suffix = ""

        mock_parent_page = {"results": [{"title": f"{prefix} {dimr_version}{suffix}", "id": ""}]}
        mock_atlassian.get_page_info_for_parent_page.return_value = mock_parent_page

        # Act & Assert
        expected_message = f"Could not find or create the page for {prefix} {dimr_version}{suffix}."
        with pytest.raises(AssertionError, match=expected_message):
            public_wiki_helper._PublicWikiHelper__get_public_wiki_page_id(parent_page_id, dimr_version, prefix, suffix)

    def test_update_content_of_page_success(self, public_wiki_helper: PublicWikiHelper, mock_atlassian: Mock) -> None:
        """Test successful page content update."""
        # Arrange
        page_id = "page_123"
        page_title = "Test Page"
        content = "<html>Test content</html>"
        mock_atlassian.update_page.return_value = True

        # Act
        public_wiki_helper._PublicWikiHelper__update_content_of_page(page_id, page_title, content)

        # Assert
        mock_atlassian.update_page.assert_called_once_with(page_id=page_id, page_title=page_title, content=content)

    def test_update_content_of_page_raises_assertion_error_on_failure(
        self, public_wiki_helper: PublicWikiHelper, mock_atlassian: Mock
    ) -> None:
        """Test that AssertionError is raised when page update fails."""
        # Arrange
        page_id = "page_123"
        page_title = "Test Page"
        content = "<html>Test content</html>"
        mock_atlassian.update_page.return_value = False

        # Act & Assert
        with pytest.raises(AssertionError, match="Failed to update the public wiki page."):
            public_wiki_helper._PublicWikiHelper__update_content_of_page(page_id, page_title, content)

    @patch.object(PublicWikiHelper, "_PublicWikiHelper__prepare_content_for_sub_page")
    @patch.object(PublicWikiHelper, "_PublicWikiHelper__get_public_wiki_page_id")
    @patch.object(PublicWikiHelper, "_PublicWikiHelper__update_content_of_page")
    def test_update_sub_page_success(
        self,
        mock_update_content: Mock,
        mock_get_page_id: Mock,
        mock_prepare_content: Mock,
        public_wiki_helper: PublicWikiHelper,
    ) -> None:
        """Test successful sub page update."""
        # Arrange
        parent_page_id = "parent_123"
        content = "<pre>Test results</pre>"
        subpage_id = "subpage_456"
        expected_title = f"{DIMR_SUBPAGE_PREFIX} 2.13.03{DIMR_SUBPAGE_SUFFIX}"

        mock_prepare_content.return_value = content
        mock_get_page_id.return_value = subpage_id

        # Act
        public_wiki_helper._PublicWikiHelper__update_sub_page(parent_page_id)

        # Assert
        mock_prepare_content.assert_called_once()
        mock_get_page_id.assert_called_once_with(
            parent_page_id=parent_page_id,
            dimr_version="2.13.03",
            prefix=DIMR_SUBPAGE_PREFIX,
            suffix=DIMR_SUBPAGE_SUFFIX,
        )
        mock_update_content.assert_called_once_with(page_id=subpage_id, page_title=expected_title, content=content)

    @patch("builtins.open", new_callable=mock_open, read_data=b"Test results content")
    def test_prepare_content_for_sub_page_success(self, mock_file: Mock, public_wiki_helper: PublicWikiHelper) -> None:
        """Test successful preparation of sub page content."""
        # Act
        result = public_wiki_helper._PublicWikiHelper__prepare_content_for_sub_page()

        # Assert
        expected_content = "<pre>Test results content</pre>"
        assert result == expected_content
        mock_file.assert_called_once_with(PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT, "rb")


class TestUpdatePublicWiki:
    """Test cases for update_public_wiki function."""

    @pytest.fixture
    def mock_context(self) -> Mock:
        """Create a mock DimrAutomationContext."""
        context = Mock(spec=DimrAutomationContext)
        context.dry_run = False
        context.get_dimr_version.return_value = "2.13.03"
        context.build_id = "123456"
        context.atlassian = Mock(spec=Atlassian)
        context.teamcity = Mock(spec=TeamCity)
        return context

    @patch("ci_tools.dimrset_delivery.update_public_wiki.PublicWikiHelper")
    def test_update_public_wiki_success(self, mock_helper_class: Mock, mock_context: Mock) -> None:
        """Test successful public wiki update."""
        # Arrange
        mock_helper_instance = Mock()
        mock_helper_class.return_value = mock_helper_instance

        # Act
        update_public_wiki(mock_context)

        # Assert
        mock_context.print_status.assert_called_once_with("Updating public wiki...")
        mock_context.get_dimr_version.assert_called_once()
        mock_helper_class.assert_called_once_with(
            atlassian=mock_context.atlassian,
            teamcity=mock_context.teamcity,
            dimr_version="2.13.03",
        )
        mock_helper_instance.update_public_wiki.assert_called_once_with("123456")

    @patch("builtins.print")
    def test_update_public_wiki_dry_run(self, mock_print: Mock, mock_context: Mock) -> None:
        """Test public wiki update in dry run mode."""
        # Arrange
        mock_context.dry_run = True
        mock_context.get_dimr_version.return_value = "2.13.03"

        # Act
        update_public_wiki(mock_context)

        # Assert
        mock_context.print_status.assert_called_once_with("Updating public wiki...")
        mock_context.get_dimr_version.assert_called_once()
        mock_print.assert_called_once_with(f"{DRY_RUN_PREFIX} Would update public wiki for DIMR version:", "2.13.03")

    def test_update_public_wiki_raises_value_error_when_atlassian_is_none(self, mock_context: Mock) -> None:
        """Test that ValueError is raised when Atlassian client is not initialized."""
        # Arrange
        mock_context.atlassian = None

        # Act & Assert
        with pytest.raises(ValueError, match="Atlassian client is required but not initialized"):
            update_public_wiki(mock_context)

    def test_update_public_wiki_raises_value_error_when_teamcity_is_none(self, mock_context: Mock) -> None:
        """Test that ValueError is raised when TeamCity client is not initialized."""
        # Arrange
        mock_context.teamcity = None

        # Act & Assert
        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            update_public_wiki(mock_context)
