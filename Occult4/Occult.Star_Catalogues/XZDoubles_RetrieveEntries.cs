using System;
using System.Collections.Generic;
using System.IO;

namespace Occult.Star_Catalogues
{
	internal class XZDoubles_RetrieveEntries
	{
		internal static List<XZDoubles> XZdoublesAccess = new List<XZDoubles>();

		internal static void PopulateList()
		{
			if (XZdoublesAccess.Count >= 100)
			{
				return;
			}
			XZdoublesAccess.Clear();
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\XZDoubles_OCC_additions.dat"))
			{
				do
				{
					XZDoubles xZDoubles = new XZDoubles();
					xZDoubles.DecodeLine(streamReader.ReadLine());
					XZdoublesAccess.Add(xZDoubles);
				}
				while (!streamReader.EndOfStream);
			}
			XZdoublesAccess.Sort();
		}

		internal static string XZdoubleEntries(double RA_Hrs, double Dec_Deg)
		{
			PopulateList();
			string text = "";
			double num = 0.00025;
			double num2 = 0.025;
			int num3 = XZdoublesAccess.Count;
			int num4 = 0;
			int num5 = 0;
			_ = RA_Hrs - num;
			_ = 0.0;
			do
			{
				num5 = (num3 + num4) / 2;
				if (XZdoublesAccess[num5].RA_Hours == RA_Hrs)
				{
					break;
				}
				if (XZdoublesAccess[num5].RA_Hours < RA_Hrs)
				{
					num4 = num5 + 1;
				}
				else
				{
					num3 = num5 - 1;
				}
			}
			while (num3 > num4);
			while (!(XZdoublesAccess[num5].RA_Hours - RA_Hrs > num))
			{
				if (Math.Abs(XZdoublesAccess[num5].RA_Hours - RA_Hrs) < num && Math.Abs(XZdoublesAccess[num5].Dec_Degrees - Dec_Deg) < num2)
				{
					text = text + XZdoublesAccess[num5].ToString().Substring(0, 97) + "\r\n";
				}
				num5++;
				if (num5 >= XZdoublesAccess.Count)
				{
					break;
				}
			}
			return text;
		}
	}
}
