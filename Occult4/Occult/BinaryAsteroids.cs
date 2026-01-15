using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Occult
{
	internal class BinaryAsteroids
	{
		public static string SourceFile = Utilities.AppPath + "\\Resource Files\\BinaryAsteroids.csv";

		public static List<BinaryAsteroidElements> BinElements = new List<BinaryAsteroidElements>();

		private static int RecordOfMaxNumberedAsteroid = 0;

		internal static DisplayData BinaryAsteroidsDisplay;

		public static void Fill_AllAsteroids()
		{
			BinElements.Clear();
			if (!File.Exists(SourceFile))
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(SourceFile);
			streamReader.ReadLine();
			while (!streamReader.EndOfStream)
			{
				BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
				binaryAsteroidElements.ReadCSVElementLine(streamReader.ReadLine());
				BinElements.Add(binaryAsteroidElements);
				if (binaryAsteroidElements.IDAsteroidNumber > 0.0)
				{
					RecordOfMaxNumberedAsteroid = BinElements.Count - 1;
				}
			}
		}

		public static void GetAsteroidRecord_fromNumberName(int AsteroidNumber, string AsteroidID, List<int> RecNums)
		{
			if (BinElements.Count < 1)
			{
				Fill_AllAsteroids();
			}
			RecNums.Clear();
			string text = AsteroidID.ToUpper();
			for (int i = 0; i < BinElements.Count; i++)
			{
				if ((BinElements[i].IDAsteroidNumber == (double)AsteroidNumber && AsteroidNumber > 0) | (BinElements[i].IDAsteroidIdentifier.ToUpper() == text))
				{
					RecNums.Add(i);
				}
			}
		}

		public static bool BinaryAsteroidPresent(int AsteroidNumber)
		{
			if (BinElements.Count < 1)
			{
				Fill_AllAsteroids();
			}
			int num = 0;
			int num2 = RecordOfMaxNumberedAsteroid;
			do
			{
				int num3 = (num + num2) / 2;
				if (BinElements[num3].IDAsteroidNumber == (double)AsteroidNumber)
				{
					return true;
				}
				if (BinElements[num3].IDAsteroidNumber < (double)AsteroidNumber)
				{
					num = num3 + 1;
				}
				else
				{
					num2 = num3 - 1;
				}
			}
			while (num <= num2);
			return false;
		}

		internal static void BinaryAsteroidDetails(int AsteroidNumber, string AsteroidID)
		{
			List<int> list = new List<int>();
			string text = "";
			int width = 330;
			if (BinaryAsteroidPresent(AsteroidNumber))
			{
				GetAsteroidRecord_fromNumberName(AsteroidNumber, AsteroidID, list);
				text += string.Format("Known binary asteroid with {0,1} satellites\r\n", list.Count);
				for (int i = 0; i < list.Count; i++)
				{
					text = text + string.Format("\r\nSatellite #{0,1}\r\n", i + 1) + BinElements[list[i]].MultiLineElements();
				}
				text = text + "".PadRight(35, '_') + "\r\n";
			}
			int gaiaBinary = GaiaBinaries.GetGaiaBinary(AsteroidNumber);
			if (gaiaBinary >= 0)
			{
				text += GaiaBinaries.GaiaBinaryData[gaiaBinary].ToString();
			}
			try
			{
				((Control)BinaryAsteroidsDisplay).Show();
			}
			catch
			{
				BinaryAsteroidsDisplay = new DisplayData();
				((Control)BinaryAsteroidsDisplay).set_Text("Binary asteroid details");
				((Control)BinaryAsteroidsDisplay).Show();
			}
			((Control)BinaryAsteroidsDisplay).set_Width(width);
			((Control)BinaryAsteroidsDisplay).set_Height(300);
			((Control)BinaryAsteroidsDisplay.txtBox).set_Text(text);
		}
	}
}
