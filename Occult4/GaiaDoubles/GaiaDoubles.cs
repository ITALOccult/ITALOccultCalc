using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Occult;

namespace GaiaDoubles
{
	internal class GaiaDoubles
	{
		internal static List<long> Index = new List<long>();

		internal static bool Initialise_Index()
		{
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Doubles.inx"))
			{
				if (!Utilities.InternetIsAvailable())
				{
					return false;
				}
				http.GetGaiaDoublesFile();
			}
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Doubles.inx"))
			{
				return false;
			}
			if (Index.Count == 0)
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Doubles.inx");
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					Index.Add(long.Parse(array[1]));
				}
				while (!streamReader.EndOfStream);
			}
			return true;
		}

		internal static bool GaiaDouble_Match(double RAdeg, double DecDeg, bool GaiaCatIDOnly, bool DetailsOnly, out string FullDetails)
		{
			FullDetails = "";
			double num = 0.001;
			if (!Initialise_Index())
			{
				return false;
			}
			int num2 = (int)(RAdeg * 4.0) - 1;
			if (num2 < 0)
			{
				num2 = 0;
			}
			int num3 = num2 + 3;
			if (num3 > 1440)
			{
				num3 = 1440;
			}
			FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Doubles.csv", FileMode.Open, FileAccess.Read);
			int num4 = (int)(Index[num3] - Index[num2]);
			byte[] array = new byte[num4 + 1];
			fileStream.Seek(Index[num2], SeekOrigin.Begin);
			fileStream.Read(array, 0, num4 - 2);
			string[] array2 = Encoding.UTF8.GetString(array, 0, num4 - 2).Split(new char[1] { '\n' });
			double num5 = num / Math.Cos(DecDeg / (180.0 / Math.PI));
			for (int i = 0; i < array2.Length; i++)
			{
				string[] array3 = array2[i].Split(new char[1] { ',' });
				double.TryParse(array3[0], out var result);
				double.TryParse(array3[1], out var result2);
				if ((Math.Abs(DecDeg - result2) < num) & (Math.Abs(RAdeg - result) < num5))
				{
					Elements.SelectiveOut(array2[i], DetailsOnly: true, out var Line, out var Line2);
					if (GaiaCatIDOnly)
					{
						int num6 = Line2.LastIndexOf(" ");
						FullDetails = Line2.Substring(num6 + 1);
						return true;
					}
					if (DetailsOnly)
					{
						FullDetails = Line.Substring(34) + "\r\n" + Line2.Substring(34);
						continue;
					}
					if (FullDetails.Length < 10)
					{
						FullDetails = "Gaia doubles within 3.6 arcsecs of  " + Utilities.DEGtoDMS(RAdeg / 15.0, 2, 2, MinutesOnly: false) + ", " + Utilities.DEGtoDMS(DecDeg, 3, 1, MinutesOnly: false) + "      Epoch 2016.0 = JD 2457388.405023\r\n";
					}
					FullDetails = FullDetails + "\r\n" + Line + "\r\n" + Line2 + "\r\n";
				}
				else if (result - RAdeg > num5)
				{
					return FullDetails.Length > 20;
				}
			}
			return false;
		}
	}
}
