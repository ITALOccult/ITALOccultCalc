using System.Drawing;
using System.Drawing.Imaging;
using System.Drawing.Printing;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	internal class Output
	{
		public static string AppPath;

		public static string PrintString;

		public static Image picImage;

		private static int StartOfLine;

		private static int FilterIndex = -1;

		private static Font printFont;

		public static string SaveAppendPredictionText(string Prediction, string FileRootName, string InitialDirectory)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0055: Unknown result type (might be due to invalid IL or missing references)
			//IL_005b: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName(FileRootName.Replace("/", "_") + ".txt");
			((FileDialog)val).set_Title(FileRootName);
			val.set_OverwritePrompt(false);
			((FileDialog)val).set_InitialDirectory(InitialDirectory);
			((FileDialog)val).set_Filter("Text files (*.txt )|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				StreamWriter streamWriter = new StreamWriter(((FileDialog)val).get_FileName(), append: true);
				streamWriter.WriteLine(Prediction);
				streamWriter.WriteLine("");
				streamWriter.Close();
				InitialDirectory = Path.GetDirectoryName(((FileDialog)val).get_FileName());
			}
			return InitialDirectory;
		}

		public static string SavePredictionText(string Prediction, string FileRootName, string InitialDirectory)
		{
			return SavePredictionText(Prediction, FileRootName, InitialDirectory, CSV: false);
		}

		public static string SavePredictionTextasCSV(string Prediction, string FileRootName, string InitialDirectory)
		{
			return SavePredictionText(Prediction, FileRootName, InitialDirectory, CSV: true);
		}

		public static string SavePredictionText(string Prediction, string FileRootName, string InitialDirectory, bool CSV)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0076: Unknown result type (might be due to invalid IL or missing references)
			//IL_007c: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			if (CSV)
			{
				((FileDialog)val).set_FileName(RemoveInvalidFileNameCharacters(FileRootName) + ".csv");
			}
			else
			{
				((FileDialog)val).set_FileName(RemoveInvalidFileNameCharacters(FileRootName) + ".txt");
			}
			((FileDialog)val).set_Title(FileRootName);
			val.set_OverwritePrompt(true);
			((FileDialog)val).set_InitialDirectory(InitialDirectory);
			if (CSV)
			{
				((FileDialog)val).set_Filter("csv files (*.csv )|*.csv|All files (*.*)|*.*");
			}
			else
			{
				((FileDialog)val).set_Filter("Text files (*.txt )|*.txt|All files (*.*)|*.*");
			}
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				StreamWriter streamWriter = new StreamWriter(((FileDialog)val).get_FileName(), append: false);
				streamWriter.WriteLine(Prediction);
				streamWriter.WriteLine("");
				streamWriter.Close();
				InitialDirectory = Path.GetDirectoryName(((FileDialog)val).get_FileName());
			}
			return InitialDirectory;
		}

		public static string SaveGraphic(Image Prediction, string FileRootName, string InitialDirectory)
		{
			//IL_0019: Unknown result type (might be due to invalid IL or missing references)
			//IL_001f: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_006f: Invalid comparison between Unknown and I4
			if (FilterIndex < 1)
			{
				FilterIndex = Settings.Default.GraphicsSaveFileType + 1;
			}
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			if (string.IsNullOrEmpty(FileRootName))
			{
				FileRootName = "";
			}
			((FileDialog)val).set_FileName(RemoveInvalidFileNameCharacters(FileRootName));
			((FileDialog)val).set_Title(FileRootName);
			val.set_OverwritePrompt(true);
			((FileDialog)val).set_InitialDirectory(InitialDirectory);
			((FileDialog)val).set_Filter("JPEG|*.jpg|BMP|*.bmp|GIF|*.gif|PNG|*.png|TIFF|*.tif");
			((FileDialog)val).set_FilterIndex(4);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				FilterIndex = ((FileDialog)val).get_FilterIndex();
				switch (FilterIndex)
				{
				case 1:
					Prediction.Save(((FileDialog)val).get_FileName(), ImageFormat.Jpeg);
					break;
				case 2:
					Prediction.Save(((FileDialog)val).get_FileName(), ImageFormat.Bmp);
					break;
				case 3:
					Prediction.Save(((FileDialog)val).get_FileName(), ImageFormat.Gif);
					break;
				case 4:
					Prediction.Save(((FileDialog)val).get_FileName(), ImageFormat.Png);
					break;
				case 5:
					Prediction.Save(((FileDialog)val).get_FileName(), ImageFormat.Tiff);
					break;
				}
				InitialDirectory = Path.GetDirectoryName(((FileDialog)val).get_FileName());
			}
			return InitialDirectory;
		}

		public static void PrintPreviewText(string Prediction)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_003f: Invalid comparison between Unknown and I4
			//IL_0041: Unknown result type (might be due to invalid IL or missing references)
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a7: Unknown result type (might be due to invalid IL or missing references)
			PrintDocument printDocument = new PrintDocument();
			PrintString = Prediction;
			StartOfLine = 0;
			printFont = new Font("Courier New", 8f);
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				PrintPreviewDialog val2 = new PrintPreviewDialog();
				val2.set_Document(printDocument);
				printDocument.DefaultPageSettings.Margins.Left = 40;
				printDocument.DefaultPageSettings.Margins.Right = 40;
				printDocument.DefaultPageSettings.Margins.Top = 40;
				printDocument.DefaultPageSettings.Margins.Bottom = 40;
				printDocument.PrintPage += pd_PrintPage;
				((Form)val2).ShowDialog();
			}
		}

		public static void PrintText_Landscape(string Prediction, int FontSizePoints)
		{
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009c: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a9: Invalid comparison between Unknown and I4
			PrintString = Prediction;
			StartOfLine = 0;
			printFont = new Font("Courier New", FontSizePoints);
			PrintDocument printDocument = new PrintDocument();
			printDocument.DefaultPageSettings.Landscape = true;
			printDocument.DefaultPageSettings.Margins.Left = 20;
			printDocument.DefaultPageSettings.Margins.Right = 40;
			printDocument.DefaultPageSettings.Margins.Top = 40;
			printDocument.DefaultPageSettings.Margins.Bottom = 40;
			if (printDocument.PrinterSettings.CanDuplex)
			{
				printDocument.PrinterSettings.Duplex = Duplex.Vertical;
			}
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += pd_PrintPage;
				printDocument.Print();
			}
		}

		public static void PrintText(string Prediction)
		{
			//IL_0093: Unknown result type (might be due to invalid IL or missing references)
			//IL_0098: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ac: Invalid comparison between Unknown and I4
			PrintString = Prediction;
			StartOfLine = 0;
			printFont = new Font("Courier New", 8f);
			PrintDocument printDocument = new PrintDocument();
			printDocument.DefaultPageSettings.Landscape = false;
			printDocument.DefaultPageSettings.Margins.Left = 40;
			printDocument.DefaultPageSettings.Margins.Right = 40;
			printDocument.DefaultPageSettings.Margins.Top = 40;
			printDocument.DefaultPageSettings.Margins.Bottom = 40;
			if (printDocument.PrinterSettings.CanDuplex)
			{
				printDocument.PrinterSettings.Duplex = Duplex.Horizontal;
			}
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += pd_PrintPage;
				printDocument.Print();
			}
		}

		private static void pd_PrintPage(object sender, PrintPageEventArgs ev)
		{
			float num = 0f;
			float num2 = 0f;
			float x = ev.MarginBounds.Left;
			float num3 = ev.MarginBounds.Top;
			string text = null;
			int num4 = 0;
			int num5 = 0;
			int num6 = PrintString.Length - 1;
			num = (float)ev.MarginBounds.Height / printFont.GetHeight(ev.Graphics);
			while ((float)num4 < num && StartOfLine < num6)
			{
				num5 = PrintString.IndexOf('\r', StartOfLine);
				if (num5 == -1)
				{
					num5 = num6;
				}
				text = PrintString.Substring(StartOfLine, num5 - StartOfLine);
				num2 = num3 + (float)num4 * printFont.GetHeight(ev.Graphics);
				ev.Graphics.DrawString(text, printFont, Brushes.Black, x, num2, new StringFormat());
				num4++;
				StartOfLine = num5 + 2;
			}
			if (StartOfLine < num6)
			{
				ev.HasMorePages = true;
			}
			else
			{
				ev.HasMorePages = false;
			}
		}

		public static void PrintPicBoxImage(Image picBox)
		{
			//IL_0060: Unknown result type (might be due to invalid IL or missing references)
			//IL_0065: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Invalid comparison between Unknown and I4
			picImage = picBox;
			PrintDocument printDocument = new PrintDocument();
			printDocument.DefaultPageSettings.Landscape = false;
			printDocument.DefaultPageSettings.Margins.Left = 40;
			printDocument.DefaultPageSettings.Margins.Right = 40;
			printDocument.DefaultPageSettings.Margins.Top = 40;
			printDocument.DefaultPageSettings.Margins.Bottom = 40;
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += pd_PrintImage;
				printDocument.Print();
			}
		}

		private static void pd_PrintImage(object sender, PrintPageEventArgs ev)
		{
			ev.Graphics.DrawImage(picImage, 0, 0);
		}

		internal static string UnHide(string Text, int Key)
		{
			StringBuilder stringBuilder = new StringBuilder(Text);
			StringBuilder stringBuilder2 = new StringBuilder(Text.Length);
			for (int i = 0; i < Text.Length; i++)
			{
				char c = stringBuilder[i];
				c = (char)(c ^ Key);
				stringBuilder2.Append(c);
			}
			return stringBuilder2.ToString();
		}

		internal static string RemoveInvalidFileNameCharacters(string GName)
		{
			return Regex.Replace(GName, "\\/:*?<>|", "").Replace("  ", " ");
		}
	}
}
