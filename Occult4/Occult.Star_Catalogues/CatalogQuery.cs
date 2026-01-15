#define TRACE
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Cache;
using System.Text;
using System.Web;

namespace Occult.Star_Catalogues
{
	public static class CatalogQuery
	{
		public static string[] VIZIER_URLS = new string[9] { "vizier.u-strasbg.fr", "vizier.cfa.harvard.edu", "vizier.hia.nrc.ca", "vizier.nao.ac.jp", "vizier.iucaa.ernet.in", "data.bao.ac.cn", "vizier.ast.cam.ac.uk", "www.ukirt.jach.hawaii.edu", "vizier.inasan.ru" };

		public static List<StarEntry> GetStarsInRegion(double raHours, double deDeg, int radiusArcSec, VizierEquinox searchEquinox, string searchCatalogs)
		{
			return GetStarsInRegion(raHours, deDeg, radiusArcSec, searchEquinox, searchCatalogs, VizierMirror.France);
		}

		public static List<StarEntry> GetStarsInRegion(double raHours, double deDeg, int radiusArcSec, VizierEquinox searchEquinox, string searchCatalogs, VizierMirror mirrorToUse)
		{
			string text = "";
			if (deDeg < 0.0)
			{
				text = "-";
				deDeg = 0.0 - deDeg;
			}
			int num = (int)raHours;
			double num2 = (raHours - (double)num) * 60.0;
			int num3 = (int)num2;
			double num4 = (num2 - (double)num3) * 60.0;
			int num5 = (int)deDeg;
			double num6 = (deDeg - (double)num5) * 60.0;
			int num7 = (int)num6;
			double value = (num6 - (double)num7) * 60.0;
			string text2 = HttpUtility.UrlEncode(string.Format("{0} {1} {2} {3} {4} {5}", num.ToString("00"), num3.ToString("00"), num4.ToString("00.0"), text + num5.ToString("00"), Math.Abs(num7).ToString("00"), Math.Abs(value).ToString("00.0")));
			try
			{
				HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create($"http://{VIZIER_URLS[(int)mirrorToUse]}/viz-bin/votable");
				httpWebRequest.Method = "POST";
				httpWebRequest.ContentType = "application/x-www-form-urlencoded";
				httpWebRequest.CachePolicy = new RequestCachePolicy(RequestCacheLevel.NoCacheNoStore);
				string text3 = HttpUtility.UrlEncode(radiusArcSec.ToString("0").PadLeft(3));
				string text4 = "-to=2&-from=-1&-this=-1&-source=&-words=&-c=" + text2 + "&-c.eq=" + searchEquinox.ToString() + "&-oc.form=sexa&-c.r=" + text3 + "&-c.u=arcsec&-c.geom=r&-meta.foot=1&-meta=1&-meta.ucd=u&%21-4c%3B=Find+Data&-out.all&-out.max=50&-out.form=VOTable&-out.add=_r&-out.add=_RA*-c.eq%2C_DE*-c.eq&-sort=_r";
				if (!string.IsNullOrEmpty(searchCatalogs))
				{
					text4 = text4 + "&-source=" + searchCatalogs;
				}
				byte[] bytes = Encoding.ASCII.GetBytes(text4);
				httpWebRequest.ContentLength = bytes.Length;
				using (Stream stream = httpWebRequest.GetRequestStream())
				{
					stream.Write(bytes, 0, bytes.Length);
				}
				using TextReader textReader = new StreamReader(((HttpWebResponse)httpWebRequest.GetResponse()).GetResponseStream());
				string text5 = textReader.ReadToEnd();
				using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Downloaded Files\\Vizer data.xml"))
				{
					streamWriter.Write(text5);
				}
				return Votable.ParseStarEntries(text5);
			}
			catch (Exception ex)
			{
				Trace.WriteLine(ex.ToString());
			}
			return new List<StarEntry>();
		}
	}
}
