using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using Occult;
using Occult.Properties;

namespace GifCreator
{
	public class GifCreator
	{
		public static void Create_Animated_Gif(List<string> GIF_Files, string DefaultOutputFile)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0054: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName(DefaultOutputFile);
			((FileDialog)val).set_Title("Save animated GIF");
			val.set_OverwritePrompt(true);
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Predictions\\");
			((FileDialog)val).set_Filter("GIF|*.gif");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			int delay = (int)(Settings.Default.GIFdelay * 100m);
			int numberOfRepeatings = (int)Settings.Default.GIFRepeats;
			BinaryWriter binaryWriter = new BinaryWriter(new FileStream(((FileDialog)val).get_FileName(), FileMode.Create, FileAccess.ReadWrite));
			byte[] buffer = new byte[6] { 71, 73, 70, 56, 57, 97 };
			binaryWriter.Write(buffer);
			for (int i = 0; i <= GIF_Files.Count; i++)
			{
				int num = i;
				if (i == GIF_Files.Count)
				{
					num = i - 1;
				}
				GifClass gifClass = new GifClass();
				gifClass.LoadGifPicture(GIF_Files[num]);
				if (num == 0)
				{
					binaryWriter.Write(gifClass.m_ScreenDescriptor.ToArray());
				}
				binaryWriter.Write(CreateGraphicControlExtensionBlock(delay));
				binaryWriter.Write(gifClass.m_ImageDescriptor.ToArray());
				binaryWriter.Write(gifClass.m_ColorTable.ToArray());
				binaryWriter.Write(gifClass.m_ImageData.ToArray());
			}
			binaryWriter.Write(CreateLoopBlock(numberOfRepeatings));
			binaryWriter.Write((byte)59);
			binaryWriter.Close();
			Cursor.set_Current(Cursors.get_Default());
		}

		public static byte[] CreateGraphicControlExtensionBlock(int delay)
		{
			byte[] array = new byte[8];
			byte b = (byte)(delay % 256);
			byte b2 = (byte)(delay / 256);
			array[0] = 33;
			array[1] = 249;
			array[2] = 4;
			array[3] = b2;
			array[4] = b;
			array[5] = 0;
			array[6] = 0;
			array[7] = 0;
			return array;
		}

		public static byte[] CreateLoopBlock()
		{
			return CreateLoopBlock(0);
		}

		public static byte[] CreateLoopBlock(int numberOfRepeatings)
		{
			byte b = (byte)(numberOfRepeatings % 256);
			byte b2 = (byte)(numberOfRepeatings / 256);
			return new byte[19]
			{
				33, 255, 11, 78, 69, 84, 83, 67, 65, 80,
				69, 50, 46, 48, 3, 1, b, b2, 0
			};
		}
	}
}
