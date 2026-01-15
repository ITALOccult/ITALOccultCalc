using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Occult
{
	public class AsteroidElements_All
	{
		public string SourceFile = Utilities.AppPath + "\\Resource Files\\AsteroidElements.csv";

		public List<AsteroidElements> AstElements = new List<AsteroidElements>();

		public AsteroidElements_All(string SF)
		{
			SourceFile = SF;
		}

		public void Fill_AllAsteroids()
		{
			AstElements.Clear();
			bool flag = true;
			if (!File.Exists(SourceFile))
			{
				return;
			}
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum((int)(new FileInfo(SourceFile).Length / 90));
			((Control)pBar).set_Text("Loading Asteroid orbital elements");
			((Control)pBar).Show();
			int num = 0;
			using (StreamReader streamReader = new StreamReader(SourceFile))
			{
				string text = streamReader.ReadLine();
				flag = text.Contains(",");
				if (!flag)
				{
					AsteroidElements asteroidElements = new AsteroidElements();
					asteroidElements.ReadElementLine(text);
					AstElements.Add(asteroidElements);
				}
				while (!streamReader.EndOfStream)
				{
					num++;
					pBar.pBarFTP.set_Value(num);
					Application.DoEvents();
					AsteroidElements asteroidElements = new AsteroidElements();
					string text2 = streamReader.ReadLine();
					if (text2.Trim().Length > 50)
					{
						if (flag)
						{
							asteroidElements.ReadCSVElementLine(text2);
						}
						else
						{
							asteroidElements.ReadElementLine(streamReader.ReadLine());
						}
						AstElements.Add(asteroidElements);
					}
				}
			}
			((Form)pBar).Close();
		}

		public int GetAsteroidRecord_fromNumber(int AsteroidNumber)
		{
			return GetAsteroidRecord_fromNumber(AsteroidNumber, GetNextHigher: false, GetNextLower: false);
		}

		public int GetAsteroidRecord_fromNumber(int AsteroidNumber, bool GetNextHigher, bool GetNextLower)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			if (AstElements.Count < 1)
			{
				Fill_AllAsteroids();
			}
			int result = -1;
			num = AstElements.Count - 1;
			do
			{
				num3 = (num + num2) / 2;
				if (AstElements[num3].IDNumber == AsteroidNumber)
				{
					return num3;
				}
				if ((AsteroidNumber < AstElements[num3].IDNumber) | (AstElements[num3].IDNumber == 0))
				{
					num = num3 - 1;
				}
				else
				{
					num2 = num3 + 1;
				}
			}
			while (num2 <= num);
			if (GetNextHigher)
			{
				if ((AsteroidNumber > AstElements[num3].IDNumber) & (num3 < AstElements.Count - 2))
				{
					return num3 + 1;
				}
				if (AsteroidNumber < AstElements[num3].IDNumber)
				{
					return num3;
				}
				return result;
			}
			if (GetNextLower)
			{
				if (AsteroidNumber < AstElements[num3].IDNumber && num3 > 0)
				{
					return num3 - 1;
				}
				if (AsteroidNumber > AstElements[num3].IDNumber)
				{
					return num3;
				}
				return result;
			}
			return result;
		}

		public string GetAsteroidName_fromNumber(int AsteroidNumber, out double PeakUncertaintyError)
		{
			PeakUncertaintyError = 0.0;
			int asteroidRecord_fromNumber = GetAsteroidRecord_fromNumber(AsteroidNumber);
			if (asteroidRecord_fromNumber < 0)
			{
				return "";
			}
			PeakUncertaintyError = AstElements[asteroidRecord_fromNumber].PeakEphemUncert;
			return AstElements[asteroidRecord_fromNumber].IDName;
		}

		public int GetAsteroidRecord_fromName(string AsteroidID)
		{
			if (AstElements.Count < 1)
			{
				Fill_AllAsteroids();
			}
			int result = -1;
			AsteroidID = AsteroidID.ToUpper();
			for (int i = 0; i < AstElements.Count; i++)
			{
				if (AstElements[i].IDName.Trim().ToUpper() == AsteroidID)
				{
					return i;
				}
			}
			return result;
		}
	}
}
