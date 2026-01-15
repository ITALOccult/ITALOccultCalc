using System;
using System.IO;
using System.Windows.Forms;
using Ionic.Zip;

namespace Occult.File_Actions
{
	public class BackupOccult
	{
		public static void BackupOccultSourceCode(string SourceDirectory, string BackupDirectory)
		{
			string text = Path.Combine(BackupDirectory, "OccultBackup v" + Utilities.OccultVersion_Short + " " + DateTime.Now.Year + Utilities.ShortMonths[DateTime.Now.Month] + DateTime.Now.Day.ToString().PadLeft(2, '0') + ".zip");
			if (!Directory.Exists(BackupDirectory))
			{
				Directory.CreateDirectory(BackupDirectory);
			}
			if (File.Exists(text))
			{
				File.Delete(text);
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			using ZipFile zipFile = new ZipFile(text);
			string[] array = new string[5] { "*.htm", "*.png", "*.jpg", "*.png", "*.hh*" };
			foreach (string searchPattern in array)
			{
				string[] files = Directory.GetFiles(SourceDirectory + "\\Help", searchPattern, SearchOption.AllDirectories);
				foreach (string fileName in files)
				{
					try
					{
						zipFile.AddFile(fileName);
					}
					catch
					{
					}
				}
			}
			array = new string[17]
			{
				"*.bmp", "*.config", "*.cs", "*.csproj", "*.jpg", "*.gif", "*.ico", "*.manifest", "*.pfx", "*.png",
				"*.resx", "*.settings", "*.sln", "*.suo", "*.txt", "*.user", "*.vdproj"
			};
			foreach (string searchPattern2 in array)
			{
				string[] files = Directory.GetFiles(SourceDirectory + "\\Occult.SDK\\", searchPattern2, SearchOption.AllDirectories);
				foreach (string fileName2 in files)
				{
					try
					{
						zipFile.AddFile(fileName2);
					}
					catch
					{
					}
				}
				files = Directory.GetFiles(SourceDirectory + "\\Utilities", searchPattern2, SearchOption.AllDirectories);
				foreach (string fileName3 in files)
				{
					try
					{
						zipFile.AddFile(fileName3);
					}
					catch
					{
					}
				}
				files = Directory.GetFiles(SourceDirectory + "\\Occult\\Occult4 Installer", searchPattern2, SearchOption.AllDirectories);
				foreach (string fileName4 in files)
				{
					try
					{
						zipFile.AddFile(fileName4);
					}
					catch
					{
					}
				}
				files = Directory.GetFiles(SourceDirectory + "\\Occult\\Updater", searchPattern2, SearchOption.AllDirectories);
				foreach (string fileName5 in files)
				{
					try
					{
						zipFile.AddFile(fileName5);
					}
					catch
					{
					}
				}
				files = Directory.GetFiles(SourceDirectory + "\\Occult\\WindowsApplication1\\", searchPattern2, SearchOption.AllDirectories);
				foreach (string fileName6 in files)
				{
					try
					{
						zipFile.AddFile(fileName6);
					}
					catch
					{
					}
				}
			}
			zipFile.Save();
			Cursor.set_Current(Cursors.get_Default());
		}
	}
}
