using System;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using Ionic.Zip;

namespace Occult
{
	public class Unzip
	{
		private static long totalBytesToExtract;

		private static long bytesExtracted;

		private static long bytesExtractedCurrentFile;

		private static int currPerc;

		private static PBar PB;

		public static bool UnZip(string SourceFile, string DestinationDirectory, string ExtractOnlyThisFile1, string ExtractOnlyThisFile2)
		{
			//IL_01ef: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				using ZipFile zipFile = new ZipFile(SourceFile);
				zipFile.ExtractProgress += zf_ExtractProgress;
				totalBytesToExtract = zipFile.EntriesSorted.Where((ZipEntry x) => !x.IsDirectory).Sum((ZipEntry x) => x.UncompressedSize);
				bytesExtracted = 0L;
				bytesExtractedCurrentFile = 0L;
				PB = new PBar();
				PB.pBarFTP.set_Minimum(0);
				PB.pBarFTP.set_Maximum(101);
				((Control)PB).Show();
				foreach (ZipEntry item in zipFile.EntriesSorted.Where((ZipEntry x) => !x.IsDirectory))
				{
					string text = item.FileName.Replace("/", "\\");
					if (!((ExtractOnlyThisFile1.Length > 0) | (ExtractOnlyThisFile2.Length > 0)) || !((text.ToUpper() != ExtractOnlyThisFile1.ToUpper()) & (text.ToUpper() != ExtractOnlyThisFile2.ToUpper())))
					{
						int num = text.LastIndexOf('\\');
						string text2 = DestinationDirectory;
						if (num != -1)
						{
							text2 = DestinationDirectory + text.Substring(0, num);
							text = text.Substring(num + 1);
						}
						using FileStream stream = new FileStream(text2 + "\\" + text, FileMode.Create, FileAccess.Write);
						item.Extract(stream);
					}
				}
				((Form)PB).Close();
				return true;
			}
			catch (Exception ex)
			{
				MessageBox.Show("The following error occurred when trying to unzip the downloaded file\r\n\r\n" + ex.Message + "\r\n\r\nThe most likely cause is that the file has not downloaded correctly.\r\n\r\nThe downloaded file will now be deleted. You will have to try downloading it again", "Download/Unzip error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				File.Delete(SourceFile);
				return false;
			}
		}

		private static void UpdateProgress()
		{
			int num = (int)Math.Floor((double)(bytesExtracted + bytesExtractedCurrentFile) * 100.0 / (double)totalBytesToExtract);
			if (currPerc != num && currPerc != num)
			{
				currPerc = num;
				PB.pBarFTP.set_Value(currPerc);
			}
		}

		private static void zf_ExtractProgress(object sender, ExtractProgressEventArgs e)
		{
			if (e.EventType == ZipProgressEventType.Extracting_BeforeExtractEntry)
			{
				bytesExtracted += bytesExtractedCurrentFile;
				bytesExtractedCurrentFile = 0L;
				((Control)PB).set_Text("Extracting " + e.CurrentEntry.FileName);
			}
			if (e.EventType == ZipProgressEventType.Extracting_EntryBytesWritten)
			{
				bytesExtractedCurrentFile = e.BytesTransferred;
				UpdateProgress();
			}
			if (e.EventType == ZipProgressEventType.Extracting_AfterExtractAll)
			{
				bytesExtracted = totalBytesToExtract;
				UpdateProgress();
			}
			if (e.EventType == ZipProgressEventType.Extracting_BeforeExtractAll)
			{
				bytesExtracted = 0L;
				bytesExtractedCurrentFile = 0L;
				currPerc = 0;
			}
		}
	}
}
